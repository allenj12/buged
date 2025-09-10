#!chezscheme
(import (chezscheme))

(suppress-greeting #t)

(define *curses* (load-shared-object "libcurses.dylib"))

(define initscr (foreign-procedure #f "initscr" () uptr))
(define getmaxx (foreign-procedure #f "getmaxx" (uptr) int))
(define getmaxy (foreign-procedure #f "getmaxy" (uptr) int))
(define raw (foreign-procedure #f "raw" () void))
(define start-color (foreign-procedure #f "start_color" () void))
(define init-pair (foreign-procedure #f "init_pair" (int int int) void))
(define use-default-colors (foreign-procedure #f "use_default_colors" () void))
(define color-set (foreign-procedure #f "color_set" (short void*) int))
(define noecho (foreign-procedure #f "noecho" () void))
(define getch (foreign-procedure #f "getch" () int))
(define endwin (foreign-procedure #f "endwin" () void))
(define wnoutrefresh (foreign-procedure #f "wnoutrefresh" (uptr) void))
(define doupdate (foreign-procedure #f "doupdate" () void))
(define erase (foreign-procedure #f "erase" () void))
(define curs-set (foreign-procedure #f "curs_set" (int) void))
(define move (foreign-procedure #f "move" (int int) void))
(define addch (foreign-procedure #f "addch" (int) void))
(define getcurx (foreign-procedure #f "getcurx" (uptr) int))
(define getcury (foreign-procedure #f "getcury" (uptr) int))

(define setlocale (foreign-procedure #f "setlocale" (int string) string))
(define get-wch (foreign-procedure #f "get_wch" (u8*) int))
(define setcchar (foreign-procedure #f "setcchar" (u8* u8* unsigned-int short uptr) int))
(define add-wch (foreign-procedure #f "add_wch" (u8*) int))
(define wcrtomb (foreign-procedure #f "wcrtomb" (u8* wchar_t u8*) size_t))
(define mbrtowc (foreign-procedure #f "mbrtowc" (u8* u8* size_t u8*) size_t))
(define wcwidth (foreign-procedure #f "wcwidth" (wchar_t) int))

;mbrtowc(&wc, ptr, len, &state);
;wcrtomb(mb, wc, &ps);
;(define pair-content (foreign-procedure #f "pair_content" (uptr uptr) void))
;(define refresh (foreign-procedure #f "refresh" () void))
;(define printw (foreign-procedure #f "printw" (uptr) void))
;(define clear (foreign-procedure #f "clear" () void))

;;-1 will be initialized on init
(define start-size 50)
(define size start-size)
(define buffer -1)
(define view-start 0)
(define stdscr -1)
(define max-cols -1)
(define max-rows -1)
(define file-name "")
(define gap-start 0)
(define gap-end start-size)
(define colors '#(1 2 3 4 5 0))
(define highlight-colors '#(6 7 8 9 10 33))
(define mark #f)

(define make-buffer (lambda (n) (make-immobile-bytevector n 0)))

(define-syntax define-with-state
    (lambda (stx)
        (syntax-case stx ()
        [(_ name ((sname state) ...) body)
         #`(define name ((lambda ()
                            (let* ((sname state) ...)
                                body))))])))

(define grow
    (lambda () 
        (let* ([nsize (fx+ size start-size)]
               [nbuf (make-buffer nsize)]
               [nend (fx+ gap-end start-size)]
               [end-diff (fx- size gap-end)])
                (bytevector-copy! buffer 0 nbuf 0 gap-start)
                (bytevector-copy! buffer gap-end nbuf nend end-diff)
                (set! buffer nbuf)
                (set! size nsize)
                (set! gap-end nend))))

(define move-gap 
    (lambda (amount)
        (if (fx< amount 0)
            (let ([move-dis (fxmin (fxabs amount) gap-start)])
                (set! gap-end (fx- gap-end move-dis))
                (set! gap-start (fx- gap-start move-dis))
                (bytevector-copy! buffer gap-start buffer gap-end move-dis))
            (let ([move-dis (fxmin amount (fx- size gap-end))])
                (bytevector-copy! buffer gap-end buffer gap-start move-dis)
                (set! gap-end (fx+ gap-end move-dis))
                (set! gap-start (fx+ gap-start move-dis))))))

(define utf8-size
    (lambda (b)
        (cond
            ((fx= (fxand b #x80) 0) 1)
            ((fx= (fxand b #xE0) #xC0) 2)
            ((fx= (fxand b #xF0) #xE0) 3)
            ((fx= (fxand b #xF8) #xF0) 4)
            (else #f))))

(define back-char
    (lambda (idx)
        (if (fx<= idx 1)
            0
            (let loop ([i (fx1- idx)])
                (cond ((and (fx>= i gap-start)
                            (fx< i gap-end))
                        (loop gap-end))
                      ((let ([b (bytevector-u8-ref buffer i)])
                            (or
                                (fx= (fxand b #x80) 0)
                                (fx= (fxand b #xE0) #xC0)
                                (fx= (fxand b #xF0) #xE0)
                                (fx= (fxand b #xF8) #xF0)
                                (fx< i 1)))
                            i)
                      (else (loop (fx1- i))))))))

(define forward-char
    (lambda (idx)
            (if (fx>= idx size)
                idx
                (fx+ idx (utf8-size (bytevector-u8-ref buffer idx))))))

(define move-back (lambda () (move-gap (fx- (back-char gap-start) gap-start))))

(define move-forward (lambda () (move-gap (fx- (forward-char gap-end) gap-end))))

(define line-start
    (lambda (s)
        (let loop ([i s])
            (if (and (fx> i 0)
                     (not (fx= (utf8-ref buffer (back-char i)) 10)))
                (loop (back-char i))
                i))))

(define line-start-count
    (lambda (s)
        (let loop ([i s]
                   [c 0])
            (if (and (fx> i 0)
                     (not (fx= (utf8-ref buffer (back-char i)) 10)))
                (loop (back-char i) (fx1+ c))
                (values i c)))))

(define line-end
    (lambda (s)
        (let loop ([i s])
            (if (and (fx< i size)
                     (not (fx= (utf8-ref buffer i) 10)))
                (loop (forward-char i))
                i))))

(define move-up
    (lambda (s)
        (let-values ([(ls count) (line-start-count s)])
            (let loop ([i (line-start (back-char ls))]
                       [c 0])
                    (if (or (fx>= c count)
                            (fx= (utf8-ref buffer i) 10))
                        i
                        (loop (forward-char i) (fx1+ c)))))))

(define move-down
    (lambda (s)
        (let-values ([(ls count) (line-start-count gap-start)])
            (let* ([le (line-end s)]
                   [nls (forward-char le)])
                (if (fx= le nls)
                    s
                    (let loop ([i nls]
                            [c 0])
                            (if (or (fx>= c count)
                                    (fx>= i size)
                                    (fx= (utf8-ref buffer i) 10))
                                i
                                (loop (forward-char i) (fx1+ c)))))))))

(define page-down
    (lambda ()
        (move-gap (fx- view-start gap-start))
        (let loop ([i gap-end]
                   [counter max-rows])
            (if (fx= counter 0)
                 (move-gap (fx- i gap-end))
                (loop (move-down i) (fx1- counter))))))

(define page-up
    (lambda ()
        (let loop ([i view-start]
                   [counter (fx/ max-rows 4)])
            (if (fx= counter 0)
                 (move-gap (fx- i gap-start))
                (loop (move-up i) (fx1- counter))))))

(define-with-state insch ([mbstate (make-bytevector 128 0)]
                          [utf8 (make-bytevector 4 0)])
    (lambda (ch)
        (let ([ch (if (char? ch) ch (integer->char ch))])
            (bytevector-fill! mbstate 0)
            (bytevector-fill! utf8 0)
            (wcrtomb utf8 ch mbstate)
            (let ([csize (utf8-size (bytevector-u8-ref utf8 0))])
                (bytevector-copy! utf8 0 buffer gap-start csize)
                (set! gap-start (fx+ gap-start csize))
                (when (fx>= (fx+ 4 gap-start) gap-end)
                    (grow))))))

(define utf8-ref
    (lambda (bv idx)
        (let ([b (bytevector-u8-ref bv idx)])
            (cond
                ((fx= (fxand b #x80) 0)
                 (bytevector-u8-ref bv idx))
                ((fx= (fxand b #xE0) #xC0)
                 (bytevector-u16-ref bv idx (native-endianness)))
                ((fx= (fxand b #xF0) #xE0)
                 (bytevector-u24-ref bv idx (native-endianness)))
                ((fx= (fxand b #xF8) #xF0)
                 (bytevector-u32-ref bv idx (native-endianness)))))))

(define utf8-set!
    (lambda (bv idx v)
        (let ([b (bytevector-u8-ref bv idx)])
            (cond
                ((fx= (fxand b #x80) 0)
                 (bytevector-u8-set! bv idx v))
                ((fx= (fxand b #xE0) #xC0)
                 (bytevector-u16-set! bv idx v (native-endianness)))
                ((fx= (fxand b #xF0) #xE0)
                 (bytevector-u24-set! bv idx v (native-endianness)))
                ((fx= (fxand b #xF8) #xF0)
                 (bytevector-u32-set! bv idx v (native-endianness)))))))

(define delch
    (lambda ()
        (let* ([b (bytevector-u8-ref buffer (back-char gap-start))]
               [csize (utf8-size b)])
            (set! gap-start (fxmax (fx- gap-start csize) 0))
            (cond
                ((fx= (fxand b #x80) 0)
                 (bytevector-u8-set! buffer gap-start 0))
                ((fx= (fxand b #xE0) #xC0)
                 (bytevector-u16-set! buffer gap-start 0 (native-endianness)))
                ((fx= (fxand b #xF0) #xE0)
                 (bytevector-u24-set! buffer gap-start 0 (native-endianness)))
                ((fx= (fxand b #xF8) #xF0)
                 (bytevector-u32-set! buffer gap-start 0 (native-endianness)))))))

(define delete-selection
    (lambda ()
        (if (fx< mark gap-start)
            (let loop ([i mark])
                (if (fx< i gap-start)
                    (begin
                        (bytevector-u8-set! buffer i 0)
                        (loop (fx1+ i)))
                    (set! gap-start mark)))
            (let ([cap (fx+ mark (fx- gap-end gap-start))])
                (let loop ([i gap-end])
                    (if (fx< i cap)
                        (begin
                            (bytevector-u8-set! buffer i 0)
                            (loop (fx1+ i)))
                        (set! gap-end i)))))))

(define copy-selection
    (lambda ()
        (define bsize (fxabs (fx- mark gap-start)))
        (define bv (make-buffer bsize))
        (if (fx< mark gap-start)
            (bytevector-copy! buffer mark bv 0 bsize)
            (bytevector-copy! buffer gap-end bv 0 bsize))
        (open-process-ports (string-append  "{ cat <<-'EOF'\n"
                                            (bytevector->string bv (make-transcoder (utf-8-codec))) 
                                            "\nEOF\n } | perl -0777 -pe 'chop' | pbcopy")
                            'block
                            (make-transcoder (utf-8-codec)))))

;;since we are using pbcopy with copy above
;;we are going to define a custom paste to avoid
;;the extra parens we process
(define paste
    (lambda ()
        (let-values ([(a out b c) (open-process-ports "pbpaste"
                                                      'block 
                                                       (make-transcoder (utf-8-codec)))])
            (let loop ([c (get-char out)])
                (when (not (eq? c #!eof))
                    (insch c)
                    (loop (get-char out)))))))

(define-with-state view-end ([wchar (make-bytevector 4 0)]
                             [mbstate (make-bytevector 128 0)])
    (lambda ()
        (let loop ([i view-start]
                   [counter 0]
                   [lc 0])
            (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                   [utf8 (make-bytevector csize)])
                (bytevector-copy! buffer i utf8 0 csize)
                (mbrtowc wchar utf8 csize mbstate)
                (cond ((and (fx>= i gap-start)
                            (fx< i gap-end))
                       (loop gap-end counter lc))
                      ((or (fx>= i size) (fx>= counter max-rows)) i)
                      ((or (fx= (utf8-ref buffer i) 10)
                           (fx= lc (fx- max-cols (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness)))))))
                       (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i))) (fx1+ counter) 0))
                      ((fx> lc (fx- max-cols (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness))))))
                       (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i))) (fx1+ counter) 2))
                      (else (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i))) counter (fx+ lc (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness))))))))))))

(define fx-between
    (lambda (x m n)
      (or (and (fx<= x n)
               (fx>= x m))
          (and (fx>= x n)
               (fx<= x m)))))

(define bound-idx
    (lambda (idx)
        (if (fx>= idx gap-end)
            (fx- idx (fx- gap-end gap-start))
            idx)))

(define-with-state draw ([wchar (make-bytevector 4 0)]
                         [mbstate (make-bytevector 128 0)]
                         [temp-array (make-bytevector 8 0)]
                         [cchar (make-bytevector 32 0)])
    (lambda ()
        (define ve (view-end))
        (let loop ([i view-start]
                   [depth -1])
            (if (and (fx>= i gap-start)
                     (fx< i gap-end))
                (loop gap-end depth)
                (when (fx< i ve)
                    (bytevector-fill! mbstate 0)
                    (bytevector-fill! wchar 0)
                    (bytevector-fill! cchar 0)
                    (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                           [utf8 (make-bytevector csize)]
                           [colors (if (and mark 
                                            (fx-between (bound-idx i) gap-start mark))
                                      highlight-colors
                                      colors)]
                           [char (bytevector-u32-ref wchar 0 (native-endianness))])
                        (bytevector-copy! buffer i utf8 0 csize)
                        (mbrtowc wchar utf8 csize mbstate)
                        (let ([char (bytevector-u32-ref wchar 0 (native-endianness))])
                            (bytevector-u32-native-set! temp-array 0 char)
                            (cond ((and (fx> depth -1)
                                        (or (fx= char 41) (fx= char 93) (fx= char 125)))
                                    (setcchar cchar temp-array 0 (vector-ref colors 
                                                                    (fxmod depth (fx1- (vector-length colors)))) 0)
                                    (add-wch cchar)
                                    (loop (fx+ i csize) (fx1- depth)))
                                ((or (fx= char 40) (fx= char 91) (fx= char 123))
                                    (setcchar cchar temp-array 0 (vector-ref colors (fxmod (fx1+ depth)
                                                                                        (fx1- (vector-length colors)))) 0)
                                    (add-wch cchar)
                                    (loop (fx+ i csize) (fx1+ depth)))
                                (else (setcchar cchar temp-array 0 (vector-ref colors (fx1- (vector-length colors))) 0)
                                    (add-wch cchar)
                                    (loop (fx+ i csize) depth))))))))))

(define-with-state curs-yx ([wchar (make-bytevector 4 0)]
                            [mbstate (make-bytevector 128 0)])
    (lambda ()
        (let loop ([i view-start]
                    [counter 0]
                    [lc 0])
            (if (fx>= i gap-start)
                (values counter lc)
                (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                       [utf8 (make-bytevector csize)])
                    (bytevector-copy! buffer i utf8 0 csize)
                    (mbrtowc wchar utf8 csize mbstate)
                        (cond ((fx>= i gap-start) 
                            (values counter lc))
                            ((fx= (utf8-ref buffer i) 10)
                             (loop (fx+ i csize) (fx1+ counter) 0))
                            ((fx= lc (fx- max-cols (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness))))))
                             (loop (fx+ i csize) (fx1+ counter) 0))
                            ((fx> lc (fx- max-cols (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness))))))
                             (loop (fx+ i csize) (fx1+ counter) 2))
                            (else 
                             (loop (fx+ i csize) counter (fx+ lc (wcwidth (integer->char (bytevector-u32-ref wchar 0 (native-endianness)))))))))))))

(define render 
    (lambda () 
        (curs-set 0)
        (erase)
        (draw)
        (let-values ([(y x) (curs-yx)])
            (move y x))
        (wnoutrefresh stdscr)
        (doupdate)
        (curs-set 1)))

(define center
    (lambda (s counter)
        (if (fx< counter 0)
            s 
            (center (move-up s) (fx1- counter)))))

(define check-view
    (lambda ()
        (when (or (fx< gap-start view-start) (fx>= gap-end (view-end)))
            (set! view-start (line-start (center gap-start (fx/ max-rows 2)))))))

(define-with-state main-loop ([wint (make-bytevector 4 0)])
    (lambda ()
        (when #t
            (begin
                (render)
                (bytevector-fill! wint 0);;needed?
                (get-wch wint)
                (proc-char (bytevector-u32-native-ref wint 0))
                (check-view)
                (main-loop))
            (endwin))))
 
(define set-screen-limits 
    (lambda () 
        (set! max-rows (getmaxy stdscr))
        (set! max-cols (getmaxx stdscr))))

(define init 
    (lambda ()
        (setlocale 0 "en_US.UTF-8") ;;6 on linux for LC_ALL
        (set! stdscr (initscr))
        (raw)
        (noecho)

        (start-color)
        (use-default-colors)
        (init-pair 1 1 -1)
        (init-pair 2 2 -1)
        (init-pair 3 3 -1)
        (init-pair 4 4 -1)
        (init-pair 5 5 -1)
     
        (init-pair 6 1 7)
        (init-pair 7 2 7)
        (init-pair 8 3 7)
        (init-pair 9 4 7)
        (init-pair 10 5 7)

        (init-pair 33 -1 7)

        (set-screen-limits)))

(define write-file
        (lambda ()
          (call-with-output-file file-name
            (lambda (port)
              (let ([wchar (make-bytevector 4 0)]
                    [mbstate (make-bytevector 128 0)])
                (let loop ([i 0])
                    (if (and (fx>= i gap-start)
                             (fx< i gap-end))
                        (loop gap-end)
                    (when (fx< i size)
                        (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                               [utf8 (make-bytevector csize)])
                            (bytevector-fill! mbstate 0)
                            (bytevector-fill! wchar 0)

                            (bytevector-copy! buffer i utf8 0 csize)
                            (mbrtowc wchar utf8 csize mbstate)

                            (put-char port (integer->char (bytevector-u32-native-ref wchar 0)))
                            (loop (fx+ i csize))))))))
                'truncate)))

(define load-file 
    (lambda (filename)
        (set! file-name (car filename));;temp placement
        (if (and (not (null? filename))
                 (file-exists? (car filename)))
                (call-with-input-file (car filename)
                    (lambda (port)
                        (let ([mbstate (make-bytevector 128 0)]
                              [utf8 (make-bytevector 4 0)])
                            (set! size (fx+ start-size (port-length port)))
                            (set! buffer (make-buffer size))
                            (let loop ([ch (read-char port)]
                                       [i start-size])
                                (when (not (eq? ch #!eof))
                                    (bytevector-fill! mbstate 0)
                                    (bytevector-fill! utf8 0)
                                    (wcrtomb utf8 ch mbstate)
                                    (let ([csize (utf8-size (bytevector-u8-ref utf8 0))])
                                        (bytevector-copy! utf8 0 buffer i csize)
                                        (loop (read-char port) (fx+ i csize))))))))
            (set! buffer (make-bytevector start-size 0)))))


(meta define keymapping
    (lambda (c)
        (case c
            (backspace 127)
            (tab 9)
            (screen-resize 410)
            (else (char->integer 
                    (string-ref (symbol->string c)
                                0))))))


(meta define proc-binding
        (lambda (stx)
            (syntax-case stx ()
                [(pairs ...)
                    (map (lambda (pair)
                            (let ([b (syntax->datum (car pair))])
                                (cons 
                                    (cond
                                        ((memq 'ctrl b) 
                                         (char-
                                             (char-upcase
                                                 (string-ref
                                                     (symbol->string (car (last-pair b)))
                                                     0))
                                              #\@))
                                        (else (keymapping (car (last-pair b)))))
                                    (cdr pair))))
                    #'(pairs ...))])))

(define-syntax define-bindings
    (lambda (stx)                         
        (syntax-case stx ()
        [(_ fn-name (binding commands ...) ...)
        #`(define fn-name
            (lambda (c)
                #,(append 
                    (cons #'case 
                        (cons #'c
                            (proc-binding #'((binding commands ...) ...))))
                  #'((else (insch c))))))])))

(define-bindings proc-char
    ((backspace) (if mark (begin (delete-selection) (set! mark #f)) (delch)))
    ((ctrl d) (when (fx< gap-end size) 
                    (let ([csize (utf8-size (bytevector-u8-ref buffer gap-end))])
                        (utf8-set! buffer gap-end 0)
                        (set! gap-end (fx+ gap-end csize)))))
    ((ctrl b) (move-back))
    ((ctrl f) (move-forward))
    ((ctrl l) (set! view-start (center gap-start (fx/ max-rows 2))))
    ((ctrl e) (move-gap (fx- (line-end gap-end) gap-end)))
    ((ctrl a) (move-gap (fx- (line-start gap-start) gap-start)))
    ((ctrl p) (move-gap (fx- (move-up gap-start) gap-start)))
    ((ctrl n) (move-gap (fx- (move-down gap-end) gap-end)))
    ((ctrl v) (page-down))
    ((ctrl y) (page-up))
    ((ctrl w) (write-file))
    ((ctrl x) (begin (endwin) (exit)))
    ((ctrl ^) (if mark (set! mark #f) (set! mark gap-start)))
    ((ctrl c) (when mark (copy-selection) (set! mark #f)))
    ((ctrl k) (when mark (copy-selection) (delete-selection) (set! mark #f)))
    ((ctrl u) (paste))
    ((screen-resize) (set-screen-limits))
    ((tab) (insch #\space) (insch #\space) (insch #\space) (insch #\space))
    ((\() (insch #\() (insch #\)) (move-back)))

(scheme-start 
    (lambda x
        (dynamic-wind 
            (lambda () (init) (load-file x))
            (lambda ()  (main-loop))
            (lambda () (endwin)))))
