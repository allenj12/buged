#!chezscheme
(import (chezscheme))

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
(define refresh (foreign-procedure #f "refresh" () void))
(define clear (foreign-procedure #f "clear" () void))
(define move (foreign-procedure #f "move" (int int) void))
(define addch (foreign-procedure #f "addch" (int) void))
(define printw (foreign-procedure #f "printw" (uptr) void))
(define getcurx (foreign-procedure #f "getcurx" (uptr) int))
(define getcury (foreign-procedure #f "getcury" (uptr) int))
(define pair-content (foreign-procedure #f "pair_content" (uptr uptr) void))

;;-1 will be initialized on init
(define start-size 5)
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

(define move-back (lambda () (move-gap -1)))

(define move-forward (lambda () (move-gap 1)))

(define line-start
    (lambda (s)
        (let loop ([i s])
            (if (and (fx> i 0)
                     (not (fx= (bytevector-u8-ref buffer (fx1- i)) 10)))
                (loop (fx1- i))
                i))))

(define line-end
    (lambda (s)
        (let loop ([i s])
            (if (and (fx< i size)
                     (not (fx= (bytevector-u8-ref buffer i) 10)))
                (loop (fx1+ i))
                i))))

(define move-up
    (lambda (s)
        (let* ([ls (line-start s)]
               [pls (line-start (fxmax (fx1- ls) 0))])
            (fx+ pls (fxmin (fx- s ls)
                            (fx- (line-end pls) pls))))))

(define move-down
    (lambda (s dist)
        (let* ([le (line-end s)]
               [nls (fxmin (fx1+ le) size)])
            (if (= le nls)
                s
                (fxmin (fx+ nls dist)
                        (line-end nls))))))

(define page-down
    (lambda ()
        (move-gap (fx- view-start gap-start))
        (let loop ([i gap-end]
                   [counter max-rows])
            (if (fx= counter 0)
                 (move-gap (fx- i gap-end))
                (loop (move-down i 0) (fx1- counter))))))

(define page-up
    (lambda ()
        (let loop ([i view-start]
                   [counter (fx/ max-rows 4)])
            (if (fx= counter 0)
                 (move-gap (fx- i gap-start))
                (loop (move-up i) (fx1- counter))))))

(define insch
    (lambda (c)
        (bytevector-u8-set! buffer gap-start c)
        (set! gap-start (fx1+ gap-start)) ;;assume never at end?
        (when (fx= gap-start gap-end)
              (grow))))

(define delch
    (lambda ()
        (set! gap-start (fxmax (fx1- gap-start) 0))
        (bytevector-u8-set! buffer gap-start 0)))

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
                    (if (fx<= i cap)
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
        (open-process-ports (string-append  "printf \""
                                            (bytevector->string bv (make-transcoder (utf-8-codec))) 
                                            "\" | pbcopy")
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
                    (insch (char->integer c))
                    (loop (get-char out)))))))

(define view-end
    (lambda ()
        (let loop ([i view-start]
                   [counter 0])
            (if (and (fx>= i gap-start)
                     (fx< i gap-end))
                (loop gap-end counter)
                (cond ((or (fx>= i size) (fx>= counter max-rows)) i)
                      ((fx= (bytevector-u8-ref buffer i) 10) (loop (fx1+ i) (fx1+ counter)))
                      (else (loop (fx1+ i) counter)))))))

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

(define draw 
    (lambda ()
        (define ve (view-end))
        (let loop ([i view-start]
                   [depth -1])
            (if (and (fx>= i gap-start)
                     (fx< i gap-end))
                (loop gap-end depth)
                (when (fx< i ve)
                    (let ([char (bytevector-u8-ref buffer i)]
                          [colors (if (and mark 
                                       (fx-between (bound-idx i) gap-start mark))
                                      highlight-colors
                                      colors)])
                        (cond ((and (fx> depth -1)
                                    (or (fx= char 41) (fx= char 93) (fx= char 125)))
                                (color-set (vector-ref colors 
                                                       (fxmod depth (fx1- (vector-length colors)))) 0)
                                (addch char)
                                (loop (fx1+ i) (fx1- depth)))
                            ((or (fx= char 40) (fx= char 91) (fx= char 123))
                                (color-set (vector-ref colors (fxmod (fx1+ depth)
                                                                     (fx1- (vector-length colors)))) 0)
                                (addch char)
                                (loop (fx1+ i) (fx1+ depth)))
                            (else (color-set (vector-ref colors (fx1- (vector-length colors))) 0) 
                                  (addch char)
                                  (loop (fx1+ i) depth)))))))))

(define curs-yx
    (lambda ()
        (values
            (let loop ([i (line-end view-start)]
                       [counter 0]
                       [lc 0])
                (if (fx< i gap-start)
                    (if (or (= (bytevector-u8-ref buffer i) 10)
                            (fx>= lc (fx1- max-cols)))
                        (loop (fx1+ i) (fx1+ counter) 0)
                        (loop (fx1+ i) counter (fx1+ lc)))
                    counter))
            (fx- gap-start (line-start gap-start)))))

(define bound-yx
    (lambda (y x)
        (values y (fxmod x max-cols))))

(define render 
    (lambda () 
        (clear) 
        (draw)
        (let-values ([(y x) (call-with-values curs-yx bound-yx)])
            (move y x))
        (refresh)))

(define center
    (lambda (s counter)
        (if (fx< counter 0)
            s
            (center (move-up s) (fx1- counter)))))

(define check-view
    (lambda ()
        (when (or (fx< gap-start view-start) (fx>= gap-end (view-end)))
            (set! view-start (line-start (center gap-start (fx/ max-rows 2)))))))

(define main-loop
    (lambda ()
        (when #t
            (begin
                (render)
                (proc-char (getch))
                (check-view)
                (main-loop))
            (endwin))))
 
(define set-screen-limits 
    (lambda () 
        (set! max-rows (getmaxy stdscr))
        (set! max-cols (getmaxx stdscr))))

(define init 
    (lambda () 
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
              (let loop ([i 0])
                (when (fx< i gap-start)
                  (put-char port (integer->char (bytevector-u8-ref buffer i)))
                  (loop (fx1+ i))))
              (let loop ([i gap-end])
                (when (fx< i size)
                  (put-char port (integer->char (bytevector-u8-ref buffer i)))
                  (loop (fx1+ i)))))
            'replace)))

(define load-file
    (lambda (filename)
        (set! file-name (car filename));;temp placement
        (if (and (not (null? filename))
                (file-exists? (car filename)))
            (call-with-input-file (car filename)
                (lambda (port)
                    (set! size (fx+ start-size (port-length port)))
                    (set! buffer (make-buffer size))
                    (let loop ([i start-size])
                        (when (fx< i size)
                            (bytevector-u8-set! buffer i (char->integer (read-char port)))
                            (loop (fx1+ i))))))
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
                    (bytevector-u8-set! buffer gap-end 0)
                    (set! gap-end (fx1+ gap-end))))
    ((ctrl b) (move-back))
    ((ctrl f) (move-forward))
    ((ctrl l) (set! view-start (center gap-start (fx/ max-rows 2))))
    ((ctrl e) (move-gap (fx- (line-end gap-end) gap-end)))
    ((ctrl a) (move-gap (fx- (line-start gap-start) gap-start)))
    ((ctrl p) (move-gap (fx- (move-up gap-start) gap-start)))
    ((ctrl n) (move-gap (fx- (move-down gap-end
                                       (fx- gap-start
                                           (line-start gap-start)))
                              gap-end)))
    ((ctrl v) (page-down))
    ((ctrl y) (page-up))
    ((ctrl w) (write-file))
    ((ctrl x) (begin (endwin) (exit)))
    ((ctrl ^) (if mark (set! mark #f) (set! mark gap-start)))
    ((ctrl c) (when mark (copy-selection) (set! mark #f)))
    ((ctrl k) (when mark (copy-selection) (delete-selection) (set! mark #f)))
    ((ctrl u) (paste))
    ((screen-resize) (set-screen-limits))
    ((tab) (insch 32) (insch 32) (insch 32) (insch 32))
    ((\() (insch (char->integer #\()) (insch (char->integer #\))) (move-back)))

(scheme-start 
    (lambda x
        (begin (load-file x) (init) (main-loop))))
