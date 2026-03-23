(define buged-comment-color 32)
(define buged-text-color 39)
(define buged-paren-colors '#(31 32 33 34))
(define buged-string-color 36)

(define buged-file-extension
    (lambda ()
        (let loop ([i (fx1- (string-length buged-file-name))])
            (cond
              ((fx< i 0) "")
              ((char=? (string-ref buged-file-name i) #\.) (substring buged-file-name i (string-length buged-file-name)))
              (else (loop (fx1- i)))))))

(let ([fe (buged-file-extension)])
    (cond
        ((member fe '(".ss" ".sls" ".scm"))
         (set! buged-highlight-sections '(("\"" "\"" string) ("#|" "|#" block-comment)))
         (set! buged-token-break-list (map char->integer '(#\( #\[ #\{ #\) #\} #\] #\space #\tab #\newline #\" #\` #\@ #\' #\; #\#))))
        ((member fe '(".c" ".cpp" ".h"))
         (set! buged-highlight-sections '(("\"" "\"" string) ("/*" "*/" block-comment)))
         (set! buged-token-break-list (map char->integer '(#\( #\[ #\{ #\) #\} #\] #\space #\tab #\newline #\" #\' #\; #\* #\/))))))

(define buged-scheme-indent
    (lambda ()
        (define open-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\( #\[ #\{)))))
        (define close-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\) #\] #\})))))
        (define quoted? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\' #\` #\@)))))
        (define newline? (lambda (i) (fx= (buged-utf8-ref buged-buffer i) (char->integer #\newline))))
        (let loop ([depth 0]
                   [i (buged-check-with-gap-start (buged-back-char buged-gap-start))])
            (cond
              ((fx<= i 0) 0)
              ((and (fx>= depth 0) (open-paren? i))
               (let-values ([(ni count) (buged-line-start-count i)])
                   (cond
                     ((or (quoted? (buged-back-char i)) (open-paren? (buged-forward-char i))) (fx1+ count))
                     (else (fx+ buged-tab-size count)))))
              ((and (newline? i) (newline? (buged-forward-char i)) (newline? (buged-forward-char (buged-forward-char i)))) 0)
              ((fx= (buged-utf8-ref buged-buffer i) 10) (loop depth (buged-back-char i)))
              ((close-paren? i) (loop (fx1- depth) (buged-back-char i)))
              ((open-paren? i) (loop (fx1+ depth) (buged-back-char i)))
              (else (loop depth (buged-back-char i)))))))

(define buged-c-indent
  (lambda ()
    (let ([open-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\(  #\{))))]
          [close-paren? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\) #\}))))]
          [whitespace? (lambda (i) (memq (buged-utf8-ref buged-buffer i) (map char->integer '(#\space #\tab))))]
          [newline? (lambda (i) (fx= (buged-utf8-ref buged-buffer i) (char->integer #\newline)))])
      (let loop ([depth 0]
                 [i buged-gap-end]
                 [count #f])
        (cond
          ((and count (or (fx<= i 0) (newline? i))) (fx+ count buged-tab-size))
          ((fx<= i 0) 0)
          ((and (not count) (fx= depth 0) (open-paren? i))
           (loop depth (buged-back-char i) 0))
          ((and (not count) (close-paren? i))
           (loop (fx1- depth) (buged-back-char i) count))
          ((and (not count) (open-paren? i))
           (loop (fx1+ depth) (buged-back-char i) count))
          ((and count (not (whitespace? i)))
           (loop depth (buged-back-char i) 0))
          ((fx= (buged-utf8-ref buged-buffer i) (char->integer #\tab))
           (loop depth (buged-back-char i) (fx+ buged-tab-size count)))
          (count
           (loop depth (buged-back-char i) (fx1+ count)))
          (else (loop depth (buged-back-char i) count)))))))

(define buged-insert-indentation
    (lambda ()
        (cond
          ((and (not buged-mark) (member (buged-file-extension) '(".ss" ".sls" ".scm")))
           (buged-inschs (cons #\newline (map (lambda (e) #\space) (iota (buged-scheme-indent))))))
          ((and (not buged-mark) (member (buged-file-extension) '(".h" ".c++" ".c")))
           (buged-inschs (cons #\newline (map (lambda (e) #\space) (iota (buged-c-indent))))))
          (else (buged-insch #\newline)))))

(define buged-auto-complete
    (let ([last-word ""]
          [matches '()])
        (define end-word-tokens (map char->integer '(#\space #\newline #\( #\[ #\{ #\) #\] #\})))
        (lambda ()
            (define get-word-start
                (lambda (i)
                    (let loop ([i (buged-back-char i)])
                        (if (or (fxzero? i)
                                (memq (buged-utf8-ref buged-buffer (buged-back-char i)) end-word-tokens))
                            i
                            (loop (buged-back-char i))))))
            (define get-word-end
                (lambda (i)
                    (let loop ([i (buged-check-with-gap-start i)])
                        (if (or (fx>= i buged-size)
                                (memq (buged-utf8-ref buged-buffer i) end-word-tokens))
                            i
                            (loop (buged-forward-char i))))))
            (let ([word-start (get-word-start buged-gap-start)]
                  [bvp (open-bytevector-input-port buged-buffer (make-transcoder (utf-8-codec)))])
                (set-port-position! bvp word-start)
                (let ([prefix (get-string-n bvp (fx- buged-gap-start word-start))])
                     (cond
                      ((string=? prefix "") #f)
                      ((string=? prefix last-word)
                       (unless (null? matches)
                           (buged-delete-selection word-start)
                           (buged-inschs (string->list (car matches)))
                           (unless (memq (buged-utf8-ref buged-buffer buged-gap-end) '(32 10 40 91 123 41 93 125))
                               (buged-delete-selection (fx- (get-word-end buged-gap-start) (fx- buged-gap-end buged-gap-start))))
                           (buged-move-gap (fx+ word-start (bytevector-length (string->utf8 prefix))))
                           (set! matches (cdr matches))))
                      (else
                       (set! matches (filter 
                                        (lambda (e)
                                            (if (fx< (string-length e) (string-length prefix))
                                                #f
                                                (string=? prefix (substring e 0 (string-length prefix)))))
                                        (map symbol->string (oblist))))
                      (unless (null? matches)
                           (set! last-word prefix)
                           (set-cdr! (last-pair matches) matches)
                           (buged-delete-selection word-start)
                           (buged-inschs (string->list (car matches)))
                           (unless (memq (buged-utf8-ref buged-buffer buged-gap-end) '(32 10 40 91 123 41 93 125))
                               (buged-delete-selection (fx- (get-word-end buged-gap-start) (fx- buged-gap-end buged-gap-start))))
                           (buged-move-gap (fx+ word-start (bytevector-length (string->utf8 prefix))))
                           (set! matches (cdr matches))))))))))

(define buged-scheme-keyword-highlights
  (lambda (section)
    `(((match
         ,(lambda (str i)
            (and (not (unbox section))
                 (member str '("if" "cond" "case" "and" "or" "when" "unless" 
                               "begin" "do" "else" "=>" "syntax-case" 
                               "syntax-rules" "with-syntax" "syntax" "set!" 
                               "quote" "quasiquote" "unquote" "call/cc")))))
       (pre ,(lambda (i) (buged-set-color 35)))
       (post ,(lambda (i) (buged-set-color buged-text-color))))
      ((match
         ,(lambda (str i)
            (and (not (unbox section))
                 (member str '("define" "define-syntax" "define-record-type" 
                               "define-enumeration" "define-values" "let" "let*" 
                               "letrec" "letrec*" "let-values" "let*-values" 
                               "fluid-let" "parameterize" "lambda" "case-lambda" 
                               "library" "module" "export" "import")))))
       (pre ,(lambda (i) (buged-set-color 34)))
       (post ,(lambda (i) (buged-set-color buged-text-color)))))))
    
(define buged-c-keyword-highlights
  (lambda (section)
    `(((match
        ,(lambda (str i)
           (and (not (unbox section))
                (member
                 str 
                 '("if" "else" "switch" "case" "default" "while" "do" "for" 
                   "goto" "continue" "break" "return" "struct" "union" 
                   "enum" "typedef" "sizeof" "typeof" "static" "extern" 
                   "const" "volatile" "inline" "restrict" "register" "auto")))))
        (pre ,(lambda (i) (buged-set-color 35)))
        (post ,(lambda (i) (buged-set-color buged-text-color))))
       ((match
        ,(lambda (str i)
           (and (not (unbox section))
                (member
                 str 
                 '("int" "char" "float" "double" "void" "short" "long" 
                   "signed" "unsigned" "bool" "_Bool" "_Complex" "_Atomic"
                   "size_t" "ssize_t" "int8_t" "int16_t" "int32_t" "int64_t"
                   "uint8_t" "uint16_t" "uint32_t" "uint64_t" "intptr_t" "uintptr_t")))))
        (pre ,(lambda (i) (buged-set-color 34)))
        (post ,(lambda (i) (buged-set-color buged-text-color)))))))

(define buged-keyword-highlights
    (lambda (section)
        (let ([fe (buged-file-extension)])
            (cond
                ((member fe '(".ss" "sls" ".scm")) (buged-scheme-keyword-highlights section))
                ((member fe '(".c" ".cpp" ".h")) (buged-c-keyword-highlights section))))))

(define buged-rainbow-parens
    (lambda (section depth)
        `(((match ,(lambda (str i) (and (not (unbox section)) (fx> (unbox depth) -1) (not (buged-escaped? i)) (member str '(")" "}" "]")))))
           (pre 
               ,(lambda (i) 
                   (buged-set-color (vector-ref buged-paren-colors (fxmod (unbox depth) (vector-length buged-paren-colors))))
                   (set-box! depth (fx1- (unbox depth)))))
           (post ,(lambda (i) (buged-set-color buged-text-color)))
           (clean-up ,(lambda () (set-box! depth -1))))
          ((match ,(lambda (str i) (and (not (unbox section)) (not (buged-escaped? i)) (member str '("(" "{" "[")))))
           (pre ,(lambda (i)
                    (set-box! depth (fx1+ (unbox depth)))
                    (buged-set-color (vector-ref buged-paren-colors (fxmod (unbox depth) (vector-length buged-paren-colors))))))
           (post ,(lambda (i) (buged-set-color buged-text-color)))))))

(define buged-string-highlights
    (lambda (section)
        `((match ,(lambda (str i) (and (not (buged-escaped? i)) (member str '("\"")))))
          (pre ,(lambda (i)
                   (cond 
                       ((not (unbox section))
                        (set-box! section 'string)
                        (buged-set-color buged-string-color))
                       ((eq? (unbox section) 'string)
                        (set-box! section #f)))))
          (post ,(lambda (i) (unless (unbox section) (buged-set-color buged-text-color))))
          (clean-up ,(lambda () (set-box! section #f))))))

(define buged-block-comment-highlights
    (lambda (section)
        (define-values (ch1 ch2)
            (let ([fe (buged-file-extension)]) 
                (cond
                    ((member fe '(".ss" ".sls" ".scm")) (values "#" "|"))
                    ((member fe '(".c" ".cpp" ".h")) (values "/" "*")))))
        `(((match ,(lambda (str i) (and (not (unbox section))
                                        (not (buged-escaped? i))
                                        (buged-buffer-match? i (string->utf8 ch1))
                                        (buged-buffer-match? (buged-forward-char i) (string->utf8 ch2)))))
           (pre ,(lambda (i) (set-box! section 'block-comment) (buged-set-color buged-comment-color)))
           (post ,(lambda (i) #f)))
          ((match ,(lambda (str i) (and (eq? (unbox section) 'block-comment)
                                        (not (buged-escaped? (buged-back-char i)))
                                        (buged-buffer-match? i (string->utf8 ch1))
                                        (buged-buffer-match? (buged-back-char i) (string->utf8 ch2)))))
           (pre ,(lambda (i) (set-box! section #f)))
           (post ,(lambda (i) (unless (unbox section) (buged-set-color buged-text-color))))))))

(define buged-line-comment-highlights
    (lambda (section)
        `(((match
              ,(let ([fe (buged-file-extension)]) 
                  (cond
                      ((member fe '(".ss" ".sls" ".scm"))
                       (lambda (str i) (and (not (unbox section))
                                            (not (fx= (buged-utf8-ref buged-buffer (buged-back-char i)) 35))
                                            (not (buged-escaped? i))
                                            (string=? str ";"))))
                      ((member fe '(".c" ".cpp" ".h"))
                       (lambda (str i) (and (not (unbox section))
                                            (not (buged-escaped? i))
                                            (string=? str "/")
                                            (buged-buffer-match? (buged-forward-char i) (string->utf8 "/"))))))))
           (pre ,(lambda (i) (set-box! section 'line-comment) (buged-set-color buged-comment-color)))
           (post ,(lambda (i) #f)))
          ((match ,(lambda (str i) (and (eq? (unbox section) 'line-comment) (member str '("\n")))))
           (pre ,(lambda (i) (set-box! section #f)))
           (post ,(lambda (i) (unless (unbox section) (buged-set-color buged-text-color))))))))

(let ([fe (buged-file-extension)])
    (when (member fe '(".ss" ".sls" ".scm" ".c" ".cpp" ".h"))
        (set! buged-highlight-rules
            (let ([depth (box -1)]
                  [section (box #f)]
                  [line-comment-str (if (member fe '(".ss" ".sls" ".scm")) ";" "//")])
                (define buged-line-comment?
                    (lambda (s comment)
                        (let loop ([i (buged-back-char s)])
                            (cond
                                ((or (fx<= (buged-bound-idx i) 0)
                                     (fx= (buged-utf8-ref buged-buffer i) 10))
                                 #f)
                                ((and (not (buged-escaped? i)) (buged-buffer-match? i comment))
                                 #t)
                                (loop (buged-back-char i))))))
               `(((pre ,(lambda (i) #f)) (post ,(lambda (i) #f)) (match ,(lambda (str i) #f))
                  (start-draw ,(lambda ()
                                   (set-box! section buged-view-state)
                                   (cond
                                       ((eq? buged-view-state 'string) (buged-set-color buged-string-color))
                                       ((eq? buged-view-state 'block-comment) (buged-set-color buged-comment-color))
                                       ((and (not (unbox section)) (buged-line-comment? buged-view-start (string->utf8 line-comment-str)))
                                        (buged-set-color buged-comment-color)
                                        (set-box! section 'line-comment))
                                       (else (buged-set-color buged-text-color))))))
                 ,@(buged-keyword-highlights section)
                 ,@(buged-rainbow-parens section depth)
                 ,(buged-string-highlights section)
                 ,@(buged-block-comment-highlights section)
                 ,@(buged-line-comment-highlights section)
                 #;((match ,(lambda (str i) (and (not (unbox section)) (string->number str))))
                  (pre ,(lambda (i) (buged-set-color 36)))
                  (post ,(lambda (i) (buged-set-color buged-text-color)))))))))

(set! buged-bindings (cons (list (buged-ctrl #\w)
                                 (lambda () (define old-bg buged-bg-color)
                                            (set! buged-bg-color 40)
                                            (buged-render)
                                            (buged-write-file)
                                            (sleep (make-time 'time-duration 100000000 0))
                                            (set! buged-bg-color old-bg)))
                           buged-bindings))
    
(set! buged-bindings (cons (list #\newline
                                 (lambda ()
                                     (buged-insert-indentation)))
                           buged-bindings))

(set! buged-bindings (cons (list (buged-esc "[46;5u")
                                 (lambda ()
                                     (buged-auto-complete)))
                           buged-bindings))
