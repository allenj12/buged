#!chezscheme
(import (chezscheme))

(suppress-greeting #t)

(define *std* (load-shared-object "libSystem.dylib"))

(define-ftype winsize
    (struct
        [ws-row unsigned-short]
        [ws-col unsigned-short]
        [ws-xpixel unsigned-short]
        [ws-ypixel unsigned-short]))

(define-ftype termios
    (struct
      [c-iflag long]
      [c-oflag long]
      [c-clfag long]
      [c-lflag long]
      [cc-t (array 20 char)]
      [c-ispeed long]
      [c-ospeed long]))

(define TIOCGWINSZ 1074295912) ;;check spelling
(define ioctl (foreign-procedure (__varargs_after 2) "ioctl" (int unsigned-long (* winsize)) int))
(define tcsetattr (foreign-procedure #f "tcsetattr" (int int (* termios)) int))
(define tcgetattr (foreign-procedure #f "tcgetattr" (int (* termios)) int))

(define setlocale (foreign-procedure #f "setlocale" (int string) string))
(define sraise (foreign-procedure #f "raise" (int) void))
(define wcrtomb (foreign-procedure #f "wcrtomb" (u8* wchar_t u8*) size_t))
(define mbrtowc (foreign-procedure #f "mbrtowc" (u8* u8* size_t u8*) size_t))
(define wcwidth (foreign-procedure #f "wcwidth" (wchar_t) int))

(define-syntax define-global
    (lambda (stx)
      (syntax-case stx ()
        [(_ name body)
         (identifier? #'name)
         (with-syntax ([global-name (datum->syntax #'name `(quote ,(string->symbol
                                                              (string-append 
                                                                   "bugged-"
                                                                   (symbol->string (syntax->datum #'name))))))])
           #`(begin
               (set-top-level-value! global-name body)
               (define-syntax name
                 (make-variable-transformer
                  (lambda (st)
                    (syntax-case st (set!)
                      [(set! id value)
                       (identifier? #'id)
                       #`(set-top-level-value! global-name value)]
                      [(id a (... ...)) (identifier? #'id) #`((top-level-value global-name) a (... ...))]
                      [id (identifier? #'id)
                       #`(top-level-value global-name)]))))))])))

;;-1 will be initialized on init
(define init-size 50)
(define size init-size)
(define buffer -1)
(define view-start 0)
(define view-in-str? #f)
(define stdscr -1)
(define max-cols -1)
(define max-rows -1)
(define gap-start 0)
(define gap-end init-size)
(define mark #f)
(define undo-point '())
(define resize? #f)
(define-values (p c) (open-string-output-port))
;;user access
(define-global start-size init-size)
(define-global tab-size 4)
(define-global text-color 39)
(define-global paren-colors '#(31 32 33 34))
(define-global string-color 36)
(define-global bg-color 49)
(define-global highlight-color 47)
(define-global file-name "")
(define-global undo-list '())

(define-syntax define-with-state
    (lambda (stx)
        (syntax-case stx ()
        [(_ name ((sname state) ...) body)
         #`(define name ((lambda ()
                            (let* ((sname state) ...)
                                body))))])))

(define screen-cmd
    (lambda sequence
        (display #\esc p)
        (display #\[ p)
        (for-each (lambda (x) (display x p)) sequence)))

(define erase
    (lambda ()
        (screen-cmd #\2 #\J)))

(define move
    (lambda (y x)
        (screen-cmd y #\; x #\H)))

(define endwin
    (lambda ()
        (erase)
        (move 0 0)
        (screen-cmd #\? #\1 #\0 #\4 #\9 #\l)
        (raw 'off)
        (display (c))))

(define hide-cursor
    (lambda ()
        (screen-cmd #\? #\2 #\5 #\l)))

(define show-cursor
    (lambda ()
        (screen-cmd #\? #\2 #\5 #\h)))

(define-with-state raw ([tios (make-ftype-pointer termios (foreign-alloc (ftype-sizeof termios)))])
    (lambda opts
        (define on? (memq 'on opts))
        (define ECHO #x00000008)
        (define ISIG #x00000080)
        (define ICANON #x00000100)
        (define IXON #x00000200)
        (define IEXTEN #x00000400)
        ;(define ICRNL #x00000100)
        (define TCSAFLUSH 2)
        (tcgetattr 0 tios)
        (let ([lflag (ftype-ref termios (c-lflag) tios)]
              [iflag (ftype-ref termios (c-iflag) tios)])
            (if on?
                (begin
                    (ftype-set! termios (c-lflag) tios (fxand lflag (fxnot (fxior ECHO ICANON ISIG IEXTEN))))
                    (ftype-set! termios (c-iflag) tios (fxand iflag (fxnot (fxior IXON)))))
                (begin
                    (ftype-set! termios (c-lflag) tios (fxior lflag (fxior ECHO ICANON ISIG IEXTEN)))
                    (ftype-set! termios (c-iflag) tios (fxior iflag (fxior IXON)))))
            (tcsetattr 0 TCSAFLUSH  tios))))

(define-with-state set-screen-limits ([w (make-ftype-pointer winsize (foreign-alloc (ftype-sizeof winsize)))])
    (lambda ()
        (ioctl 0 TIOCGWINSZ w)
        (set! max-rows (ftype-ref winsize (ws-row) w))
        (set! max-cols (ftype-ref winsize (ws-col) w))))

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

(define utf8-size
    (lambda (b)
        (cond
            ((fx= (fxand b #x80) 0) 1)
            ((fx= (fxand b #xE0) #xC0) 2)
            ((fx= (fxand b #xF0) #xE0) 3)
            ((fx= (fxand b #xF8) #xF0) 4)
            (else #f))))

(define-with-state utf8-char-set! ([mbstate (make-bytevector 128 0)]
                                   [utf8 (make-bytevector 4 0)])
    (lambda (bv i ch)
        (bytevector-fill! mbstate 0)
        (bytevector-fill! utf8 0)
        (wcrtomb utf8 ch mbstate)
        (let ([csize (utf8-size (bytevector-u8-ref utf8 0))])
            (bytevector-copy! utf8 0 bv i csize)
            csize)))

(define-with-state utf8-char-ref ([mbstate (make-bytevector 128 0)])
    (lambda (bv i)
        (let* ([wchar (make-bytevector 4 0)]
               [csize (utf8-size (bytevector-u8-ref bv i))]
               [utf8 (make-bytevector csize)]) ;can lift up?
                (bytevector-copy! buffer i utf8 0 csize)
                (mbrtowc wchar utf8 csize mbstate)
                wchar)))

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

(define wchar-width
    (lambda (wchar)
        (if (fx= (bytevector-u32-native-ref wchar 0) 9)
            tab-size
            (wcwidth (integer->char (bytevector-u32-native-ref wchar 0))))))

(define back-char
    (lambda (idx)
        (if (fx<= idx 1)
            0
            (let loop ([i (fx1- idx)])
                (cond ((and (fx>= i gap-start)
                            (fx< i gap-end))
                        (if (fxzero? gap-start) gap-end (loop (fx1- gap-start))))
                      ((let ([b (bytevector-u8-ref buffer i)])
                            (or
                                (fx= (fxand b #x80) 0)
                                (fx= (fxand b #xE0) #xC0)
                                (fx= (fxand b #xF0) #xE0)
                                (fx= (fxand b #xF8) #xF0)
                                (fx< i 1)))
                            i)
                      (else (loop (fx1- i))))))))

(define back-word
    (lambda (idx)
        (let loop ([i (back-char idx)])
            (if (fx< i 1)
                i
                (let ([cur-ch (integer->char 
                                  (bytevector-u32-native-ref (utf8-char-ref buffer i) 0))]
                      [back-ch (integer->char 
                                   (bytevector-u32-native-ref (utf8-char-ref buffer 
                                                                             (back-char i))
                                                              0))])
                    (if (and (not (char-whitespace? cur-ch))
                             (char-whitespace? back-ch))
                        i
                        (loop (back-char i))))))))

(define forward-char
    (lambda (idx)
            (if (fx>= idx size)
                idx
                (fx+ idx (utf8-size (bytevector-u8-ref buffer idx))))))

(define forward-word
    (lambda (idx)
        (let loop ([i (forward-char idx)])
            (if (fx>= i size)
                i
                (let ([cur-ch (integer->char 
                                  (bytevector-u32-native-ref (utf8-char-ref buffer i) 0))]
                      [back-ch (integer->char 
                                   (bytevector-u32-native-ref (utf8-char-ref buffer 
                                                                             (back-char i))
                                                              0))])
                    (if (and (char-whitespace? cur-ch)
                             (not (char-whitespace? back-ch)))
                        i
                        (loop (forward-char i))))))))

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
                (loop (back-char i) (fx+ c (wchar-width (utf8-char-ref buffer (back-char i)))))
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
                        (loop (forward-char i) (fx+ c (wchar-width (utf8-char-ref buffer i)))))))))

;;TODO this needs to be refactored heavily and be made more efficient
;;currently does handle the wrapping when using multi-width unicode
;;Always go to the beggining of the display line, used for center for now
(define line-start-visible
    (lambda (s)
        (let-values ([(ls c) (line-start-count s)])
            (if (fx>= c max-cols)
                (let loop ([i ls]
                           [s1 ls]
                           [lc 0])
                      (if (fx>= i s)
                          s1
                          (let* ([wchar (utf8-char-ref buffer i)])
                                    (cond
                                        ((fx> lc (fx- max-cols (wchar-width wchar)))
                                         (loop i i 0)) ;;forward-char here?
                                        (else (loop (forward-char i) s1 (fx+ lc (wchar-width wchar))))))))
                ls))))

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
                                (loop (forward-char i)
                                      (fx+ c (wchar-width (utf8-char-ref buffer i)))))))))))

(define page-down
    (lambda ()
        (move-gap (fx- view-start gap-start))
        (let loop ([i gap-end]
                   [counter (fx1+ max-rows)])
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

;;TODO update this to just use string->utf8 instead.
(define inschs
    (lambda (chs)
        (for-each
            (lambda (ch)
                (set! gap-start (fx+ gap-start 
                                    (utf8-char-set! 
                                        buffer 
                                        gap-start
                                        (if (char? ch) ch (integer->char ch)))))
                (when (fx>= (fx+ 4 gap-start) gap-end)
                    (grow)))
            chs)
        (set! undo-list (cons (list gap-start 'delch (length chs)) undo-list))
        (set! undo-point undo-list)))

(define insch
    (lambda (ch)
        (inschs (list ch))))

(define delch
    (lambda ()
        (delete-selection (back-char gap-start))))

(define delete-selection
    (lambda mark?
        (let ([mark (if (null? mark?) mark (car mark?))])
            (when (not (fx= mark gap-start))
                (if (fx< mark gap-start)
                    (begin 
                        (let* ([count (fx- gap-start  mark)]
                               [bv-store (make-bytevector count 0)])
                            (bytevector-copy! buffer mark bv-store 0 count)
                            (set! undo-list (cons (list mark 'insch bv-store) undo-list)))
                        (let loop ([i mark])
                            (if (fx< i gap-start)
                                (begin
                                    (bytevector-u8-set! buffer i 0)
                                    (loop (fx1+ i)))
                                (set! gap-start mark))))
                    (begin
                        (let* ([count (fx- mark gap-start)]
                               [bv-store (make-bytevector count 0)])
                            (bytevector-copy! buffer gap-end bv-store 0 count)
                            (set! undo-list (cons (list gap-start 'insch bv-store) undo-list)))
                        (let ([cap (fx+ mark (fx- gap-end gap-start))])
                            (let loop ([i gap-end])
                                (if (fx< i cap)
                                    (begin
                                        (bytevector-u8-set! buffer i 0)
                                        (loop (fx1+ i)))
                                    (set! gap-end i))))))
                    (set! undo-point undo-list)))))

(define copy-selection
    (lambda ()
        (define esc-back-tick
            (lambda (bv)
                (let ([in-port (open-bytevector-input-port bv (make-transcoder (utf-8-codec)))])
                    (let-values ([(out-port gen-string) (open-string-output-port)])
                        (let loop ([c (read-char in-port)])
                            (if (eq? c #!eof)
                                (gen-string)
                                (begin
                                    (when (or (char=? c #\$)
                                              (char=? c #\\)
                                              (char=? c #\`))
                                        (write-char #\\ out-port))
                                    (write-char c out-port)
                                    (loop (read-char in-port)))))))))
        (define bsize (fxabs (fx- mark gap-start)))
        (define bv (make-buffer bsize))
        (if (fx< mark gap-start)
            (bytevector-copy! buffer mark bv 0 bsize)
            (bytevector-copy! buffer gap-end bv 0 bsize))
        (open-process-ports (string-append  "{ IFS= read -r -d '' bugedcopyvar<<EOF\n"
                                            (esc-back-tick bv)
                                            "\nEOF\nprintf \"%s\" \"$bugedcopyvar\"; } | perl -0777 -pe 'chop' | pbcopy")
                            'block
                            (make-transcoder (utf-8-codec)))))

;;TODO: refactor move-down/move-up to handle traversing gaps so we can reuse them
(define jump-to
    (lambda (keep-mark?)
        (define bsize (fxabs (fx- mark gap-start)))
        (define bv (make-buffer bsize))
        (if (fx< mark gap-start)
            (bytevector-copy! buffer mark bv 0 bsize)
            (bytevector-copy! buffer gap-end bv 0 bsize))
        (let ([line-num (string->number (utf8->string bv))])
            (when line-num
                (delete-selection)
                (when (not keep-mark?) (set! mark #f))
                (let loop ([ln line-num]
                           [i 0])
                    (cond 
                        ((fx>= i size)
                         (move-gap (fx- i (fx- gap-end gap-start) gap-start)))
                        ((and (fx>= i gap-start)
                              (fx< i gap-end))
                         (loop ln gap-end))
                        ((fx<= ln 1)
                         (move-gap (fx- (if (fx>= i gap-start)
                                            (fx- i (fx- gap-end gap-start))
                                            i)
                                        gap-start)))
                        ((fx= (bytevector-u8-ref buffer i) 10)
                         (loop (fx1- ln) (fx1+ i)))
                        (else
                         (loop ln (fx+ i (utf8-size (bytevector-u8-ref buffer i)))))))))))

(define buffer-match?
    (lambda (idx bv)
        (let loop ([i idx]
                   [bv-i 0])
        (cond
            ((fx>= bv-i (bytevector-length bv)) #t)
            ((or (fx>= i size) (fx<= i 0)) #f)
            ((not (fx= (utf8-ref buffer i) (utf8-ref bv bv-i)))
             #f)
            (else (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i)))
                        (fx+ bv-i (utf8-size (bytevector-u8-ref bv bv-i)))))))))

(define-with-state find-match ([bv #f]
                               [bsize 0])
    (lambda opts
        (when (and mark (memq 'store opts))
            (set! bsize (fxabs (fx- mark gap-start)))
            (set! bv (make-buffer bsize))
            (if (fx< mark gap-start)
                (bytevector-copy! buffer mark bv 0 bsize)
                (bytevector-copy! buffer gap-end bv 0 bsize))
            (delete-selection)
            (when (not (memq 'keep-mark opts)) (set! mark #f)))
            (when (and bv (not (memq 'store opts)))
                (let* ([forward? (memq 'forward opts)]
                       [incr (if forward? forward-char back-char)]
                       [end-cond (if forward?
                                    (lambda (i) (fx>= i size))
                                    (lambda (i) (fx<= i 0)))]
                       [base (if forward? gap-end gap-start)])
                    (let loop ([i (if forward? gap-end (back-char gap-start))])
                        (cond 
                            ((end-cond i)
                             #f)
                            ((buffer-match? i bv)
                             (move-gap (fx- (if forward? 
                                                (fx+ i (bytevector-length bv))
                                                i)
                                            base))
                             (when (not (memq 'keep-mark opts))
                                   (set! mark
                                       (if forward?
                                         (fx- i (fx- gap-end gap-start))
                                         (fx+ i (bytevector-length bv))))))
                            (else
                             (loop (incr i)))))))))

(define-with-state execute ([cmd #f])
    (lambda (delete-command?)
        (when mark
            (let* ([bsize (fxabs (fx- mark gap-start))]
                   [bv (make-buffer bsize)])
                (if (fx< mark gap-start)
                    (bytevector-copy! buffer mark bv 0 bsize)
                    (bytevector-copy! buffer gap-end bv 0 bsize))
                (set! cmd (utf8->string bv))
                (when delete-command? (delete-selection))))
        (when cmd
            (set! mark gap-start)
            (let-values ([(in out err id) (open-process-ports cmd
                                                          'block 
                                                          (make-transcoder (utf-8-codec)))])
                (when (not (port-eof? out)) (inschs (string->list (get-string-all out))))
                (when (not (port-eof? err)) (inschs (string->list (get-string-all err))))))))

(define scheme-execute
    (lambda ()
        (when mark
            (let* ([bsize (fxabs (fx- mark gap-start))]
                   [bv (make-buffer bsize)])
                (if (fx< mark gap-start)
                    (bytevector-copy! buffer mark bv 0 bsize)
                    (bytevector-copy! buffer gap-end bv 0 bsize))
                (let-values ([(port gen-str) (open-string-output-port)])
                    (define bv-port (open-bytevector-input-port bv (make-transcoder (utf-8-codec))))
                    (guard (x [else (set! mark gap-start) (inschs (string->list (condition-message x)))])
                        (display 
                            (let loop ([expr (read bv-port)])
                                (if (port-eof? bv-port) 
                                    (eval expr (interaction-environment))
                                    (begin 
                                        (eval expr (interaction-environment))
                                        (loop (read bv-port)))))
                            port)
                        (set! mark gap-start)
                        (inschs (string->list (gen-str)))))))))

(define undo
    (lambda ()
            (when (not (null? undo-point))
                ;;marks currently not handled properly here
                (set! mark #f)
                (let ([pos (caar undo-point)]
                      [type (cadar undo-point)]
                      [arg (caddar undo-point)]
                      [point undo-point])               
                    (move-gap (fx- pos gap-start))
                    (if (eq? type 'insch)
                        (begin 
                            (bytevector-copy! arg 0 buffer gap-start (bytevector-length arg))
                            ;;should be safe without grow since we are explicitly in undo state
                            (set! gap-start (fx+ gap-start (bytevector-length arg)))
                            (set! undo-list 
                                 (cons
                                    (list gap-start 'delch (string-length (utf8->string arg)))
                                    undo-list)))
                        (let loop ([n arg]
                                   [i gap-start])
                            (if (fx> n 0)
                                (loop (fx1- n) (back-char i))
                                (delete-selection i))))
                    (set! undo-point (cdr point))))))

;;since we are using pbcopy with copy above
;;we are going to define a custom paste to avoid
;;the extra parens we process
(define paste
    (lambda ()
        (let-values ([(a out b c) (open-process-ports "pbpaste"
                                                      'block 
                                                      (make-transcoder (utf-8-codec)))])
            (when (not (port-eof? out)) (inschs (string->list (get-string-all out)))))))

(define view-end
    (lambda ()
        (let loop ([i view-start]
                   [counter 0]
                   [lc 0])
            (cond ((and (fx>= i gap-start)
                        (fx< i gap-end))
                   (loop gap-end counter lc))
                  ((fx>= i size)
                   size)
                  (else (let* ([wchar (utf8-char-ref buffer i)])
                            (cond 
                                ((or (fx>= i size) (fx>= counter max-rows)) i)
                                ((or (fx= (utf8-ref buffer i) 10)
                                     (fx= lc (fx- max-cols (wchar-width wchar))))
                                 (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i))) (fx1+ counter) 0))
                                ((fx> lc (fx- max-cols (wchar-width wchar)))
                                 (loop i (fx1+ counter) 0))
                                (else (loop (fx+ i (utf8-size (bytevector-u8-ref buffer i))) counter (fx+ lc (wchar-width wchar)))))))))))

(define fx-between
    (lambda (x m n)
      (or (and (fx< x n);;so we do not highlight one extra char when gap-start < mark
               (fx>= x m))
          (and (fx>= x n)
               (fx<= x m)))))

(define bound-idx
    (lambda (idx)
        (if (fx>= idx gap-end)
            (fx- idx (fx- gap-end gap-start))
            idx)))

(define set-color
    (lambda (x)
        (screen-cmd x #\m)))

(define tab-string
    (lambda ()
        (list->string (map (lambda (x) #\space) (iota tab-size)))))

(define escaped?
    (lambda (idx)
        (and (fx= 92 (bytevector-u8-ref buffer (back-char idx)))
             (or (not (fx= 92 (bytevector-u8-ref buffer (back-char (back-char idx)))))
                 (fx= (back-char idx) (back-char (back-char idx)))))))

(define draw 
    (lambda ()
        (define ve (view-end))
        (when view-in-str? (set-color string-color))
        (let loop ([i view-start]
                   [depth -1]
                   [lc 0]
                   [in-str? view-in-str?])
            (if (and (fx>= i gap-start)
                     (fx< i gap-end))
                (loop gap-end depth lc in-str?)
                (if (and (fx< i size) (fx< i ve))
                    (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                           [wchar (utf8-char-ref buffer i)]
                           [char (bytevector-u32-ref wchar 0 (native-endianness))])
                        (if (and mark (fx-between (bound-idx i) gap-start mark))
                                      (set-color highlight-color)
                                      (set-color bg-color))
                        (cond ((and (fx= char 34) 
                                    (not (fx= 92 (bytevector-u8-ref buffer (back-char i)))))
                                (unless in-str? (set-color string-color))
                                (display #\" p)
                                (loop (fx+ i csize) depth (fx1+ lc) (not in-str?)))
                              ((and (not in-str?)
                                    (fx> depth -1)
                                    (or (fx= char 41) (fx= char 93) (fx= char 125))
                                    (not (escaped? i)))
                               (set-color (vector-ref paren-colors 
                                                      (fxmod depth (fx1- (vector-length paren-colors)))))
                               (display (integer->char char) p)
                               (loop (fx+ i csize) (fx1- depth) (fx1+ lc) in-str?))
                              ((and (not in-str?) 
                                    (or (fx= char 40) (fx= char 91) (fx= char 123))
                                    (not (escaped? i)))
                               (set-color (vector-ref paren-colors 
                                                      (fxmod (fx1+ depth) (fx1- (vector-length paren-colors)))))
                               (display (integer->char char) p)
                               (loop (fx+ i csize) (fx1+ depth) (fx1+ lc) in-str?))
                              ((fx= char 10)
                               (set-color bg-color)
                               (let repeat ([r (fxmod lc  max-cols)])
                                   (when (fx< r max-cols)
                                         (display #\space p)
                                         (repeat (fx1+ r))))
                               (loop (fx+ i csize) depth (fx+ lc (fx- max-cols (fxmod lc max-cols))) in-str?))
                              (else (unless in-str? 
                                        (set-color text-color))
                                    (if (and (fx> (wchar-width wchar) 1)
                                             (fx< 0 (fxmod (fx+ lc (wchar-width wchar)) max-cols) (wchar-width wchar)))
                                        ;;add dummy character for wide char wraps
                                        (begin 
                                               (let repeat ([times (fx- max-cols (fxmod lc max-cols))])
                                                   (when (fx> times 0)
                                                       (display #\space p)
                                                       (repeat (fx1- times))))
                                               (if (fx= char 9)
                                                   (display (tab-string) p)
                                                   (display (integer->char char) p))
                                               (loop (fx+ i csize) depth (fx+ lc (fx- max-cols (fxmod lc max-cols)) (wchar-width wchar)) in-str?))
                                        (begin 
                                               (if (fx= char 9)
                                                   (display (tab-string) p)
                                                   (display (integer->char char) p))
                                               (loop (fx+ i csize) depth (fx+ lc (wchar-width wchar)) in-str?))))))
                    (begin 
                        (set-color bg-color)
                        (let repeat ([r (fx+ (fx- max-cols (fxmod lc  max-cols))
                                             (fx* (fx- max-rows 1 (fx/ lc max-cols)) max-cols))])
                                       (when (fx> r 0)
                                             (display #\space p)
                                             (repeat (fx1- r)))))
)))))

(define curs-yx
    (lambda ()
        (let loop ([i view-start]
                    [counter 0]
                    [lc 0])
            (if (fx>= i gap-start)
                (values counter lc)
                (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                       [wchar (utf8-char-ref buffer i)])
                        (cond 
                            ((fx>= i gap-start) 
                             (values counter lc))
                            ((fx= (utf8-ref buffer i) 10)
                             (loop (fx+ i csize) (fx1+ counter) 0))
                            ((fx= lc (fx- max-cols (wchar-width wchar)))
                             (loop (fx+ i csize) (fx1+ counter) 0))
                            ((fx> lc (fx- max-cols (wchar-width wchar)))
                             (loop (fx+ i csize) (fx1+ counter) (wchar-width wchar)))
                            (else 
                             (loop (fx+ i csize) counter (fx+ lc (wchar-width wchar))))))))))

(define render 
    (lambda () 
        (hide-cursor)
        (move 0 0)
        (draw)
        (let-values ([(y x) (curs-yx)])
            (move (fx1+ y) (fx1+ x)))
        (show-cursor)
        (display (c))))

(define move-up-anywhere
    (lambda (s)
        (let loop ([counter 0]
                   [i (back-char s)])
            (let* ([wchar (utf8-char-ref buffer i)]
                   [wc-width (wchar-width wchar)])
                   (cond
                        ((fx<= i 0) 0)
                        ((or
                           (fx>= counter (fx- max-cols wc-width))
                           (fx= 10 (bytevector-u32-native-ref wchar 0)))
                         i)
                        (else 
                         (loop (fx+ counter wc-width) (back-char i))))))))

(define center
    (lambda (s counter)
        (if (fx< counter 0)
            (begin (set! view-start (line-start-visible s))
                   (set! view-in-str?
                       (fxodd?
                           (let loop ([i 0]
                                      [count 0])
                               (cond 
                                   ((fx>= i view-start) count)
                                   ((and (fx= 34 (bytevector-u8-ref buffer i))
                                         (not (fx= 92 (bytevector-u8-ref buffer (back-char i)))))
                                    (loop (forward-char i) (fx1+ count)))
                                   (else (loop (forward-char i) count)))))))
            (center (move-up-anywhere s) (fx1- counter)))))

(define check-view
    (lambda ()
        (when (or (fx< gap-start view-start) (let-values ([(y x) (curs-yx)]) (fx>= y max-rows)))
            (center gap-start (fx/ max-rows 2)))))


(define main-loop
    (lambda ()
        (render)
        (if resize?
            (begin
                (set! resize? #f)
                (set-screen-limits)
                (check-view)
                (main-loop))
            (begin
                (proc-char (char->integer (read-char)))
                (check-view)
                (main-loop)))))

(define init 
    (lambda ()
        (setlocale 0 "en_US.UTF-8") ;;6 on linux for LC_ALL
        (raw 'on)
        (screen-cmd #\? #\1 #\0 #\4 #\9 #\h)
        (display (c))
        (set-screen-limits)))

(define write-file
    (lambda ()
        (when (fx> (string-length file-name) 0)
            (call-with-output-file file-name
                (lambda (port)
                    (let loop ([i 0])
                        (if (and (fx>= i gap-start)
                                 (fx< i gap-end))
                            (loop gap-end)
                        (when (fx< i size)
                            (let* ([csize (utf8-size (bytevector-u8-ref buffer i))]
                                   [wchar (utf8-char-ref buffer i)])
                                (put-char port (integer->char (bytevector-u32-native-ref wchar 0)))
                                (loop (fx+ i csize)))))))
                    'truncate))))

(define load-file 
    (lambda (filename)
        (if (and (not (null? filename))
                 (file-exists? (car filename)))
                (call-with-input-file (car filename)
                    (lambda (port)
                        (set! file-name (car filename))
                        (set! size (fx+ start-size (port-length port)))
                        (set! buffer (make-buffer size))
                        (let loop ([ch (read-char port)]
                                   [i start-size])
                            (when (not (eq? ch #!eof))
                                (loop (read-char port) (fx+ i (utf8-char-set! buffer i ch)))))))
            (set! buffer (make-bytevector start-size 0)))))

(define resize-handler
    (lambda (x)
        (set! resize? #t)))

(meta define keymapping
    (lambda (c)
        (case c
            (backspace 127)
            (tab 9)
            (screen-resize 410)
            (else (char->integer 
                    (string-ref (symbol->string c)
                                0))))))


(meta define non-meta-binding
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

;;assumes ascii only meta bindings
(meta define meta-binding
        (lambda (stx)
            (syntax-case stx ()
                [(pairs ...)
                    (map (lambda (pair)
                            (let ([b (syntax->datum (car pair))])
                                (cons 
                                    (char->integer
                                         (string-ref
                                             (symbol->string (car (last-pair b)))
                                             0))
                                    (cdr pair))))
                    #'(pairs ...))])))

(define-syntax define-bindings
    (lambda (stx)
        (define meta? (lambda (pair) (memq 'meta (syntax->datum (car pair)))))
        (syntax-case stx ()
        [(_ fn-name (binding commands ...) ...)
        #`(define fn-name
            (lambda (c)
                #,(cons #'case
                        (cons #' c
                            (append 
                                (non-meta-binding 
                                    (filter (lambda (pair) (not (meta? pair)))
                                          #'((binding commands ...) ...)))
                                #`((27
                                    #,(cons #'case 
                                        (cons #'(char->integer (read-char))
                                                (meta-binding (filter meta? #'((binding commands ...) ...)))))))
                                #'((else (when (or (fx>= c 32)
                                                   (fx= c 9)
                                                   (fx= c 10)) (insch c)))))))))])))

;;TODO should probably change fn flags to keywords instead of #t/#f
(define-bindings proc-char
    ((backspace) (if mark (begin (delete-selection) (set! mark #f)) (delch)))
    ((ctrl d) (when (fx< gap-end size) 
                    (move-forward) (delch)))
    ((ctrl b) (move-back))
    ((meta b) (move-gap (fx- (back-word gap-start) gap-start)))
    ((ctrl f) (move-forward))
    ((meta f) (move-gap (fx- (forward-word gap-end) gap-end)))
    ((ctrl l) (center gap-start (fx/ max-rows 2)))
    ((ctrl e) (move-gap (fx- (line-end gap-end) gap-end)))
    ((ctrl a) (move-gap (fx- (line-start gap-start) gap-start)))
    ((ctrl p) (move-gap (fx- (move-up gap-start) gap-start)))
    ((ctrl n) (move-gap (fx- (move-down gap-end) gap-end)))
    ((ctrl v) (page-down))
    ((meta v) (page-up))
    ((ctrl w) (write-file))
    ((ctrl x) (endwin) (exit))
    ((ctrl h) (if mark (set! mark #f) (set! mark gap-start)))
    ((meta h) (if mark (set! mark #f) (set! mark gap-start))) ;;redundant to make certain sequences smoother
    ((ctrl g) (when mark (jump-to #f)))
    ((meta g) (when mark (jump-to #t)))
    ((ctrl q) (find-match 'forward))
    ((meta q) (find-match 'reverse))
    ((meta Q) (find-match 'store))
    ((ctrl r) (find-match 'forward 'keep-mark))
    ((meta r) (find-match 'reverse 'keep-mark))
    ((meta R) (find-match 'store 'keep-mark))
    ((ctrl o) (execute #t))
    ((meta o) (execute #f))
    ((meta O) (scheme-execute))
    ((ctrl s) (new-cafe))
    ((ctrl c) (when mark (copy-selection) (set! mark #f)))
    ((ctrl k) (when mark (copy-selection) (delete-selection) (set! mark #f)))
    ((ctrl y) (when mark (delete-selection)) (paste) (set! mark #f))
    ((meta y) (paste))
    ((screen-resize) (set-screen-limits))
    ((ctrl z) (endwin) (sraise 18) (init))
    ((ctrl _) (undo))
    ((tab) (inschs (map (lambda (e) #\space) (iota tab-size))))
    ((\() (insch #\() (insch #\)) (move-back))
    ((\[) (insch #\[) (insch #\]) (move-back))
    ((\{) (insch #\{) (insch #\}) (move-back)))

(scheme-start 
    (lambda x
        (dynamic-wind 
            (lambda () (register-signal-handler 28 resize-handler) (init) (load-file x))
            (lambda () (main-loop))
            (lambda () (endwin)))))
