#!/opt/homebrew/bin/chez --script

(optimize-level 3)
(debug-level 0)
(compile-imported-libraries #t)
(generate-wpo-files #t)
(compile-program "main.ss")
(compile-whole-program "main.wpo" "main.so")

(fasl-compressed #f)
(make-boot-file "main.boot" '("scheme" "petite") "main.so")
(vfasl-convert-file "main.boot" "main.boot" '("scheme" "petite"))

(define boot->cheader
    (lambda (src target name)
        (let ([in (open-file-input-port src)])
            (call-with-output-file target
                (lambda (out)
                    (fprintf out "unsigned char ~a[] = { \n" name)
                    (let loop ([byte (get-u8 in)]
                               [count 0])
                        (if (eof-object? byte)
                            (fprintf out "\n};\nunsigned int ~a_len = ~a;\n" name count)
                            (begin 
                                (fprintf out "0x~2,'0x" byte)
                                (unless (port-eof? in)
                                        (fprintf out ", "))
                                (loop (get-u8 in) (fx1+ count)))))
                    (close-port in))
                '(truncate)))))

(boot->cheader "main.boot" "main_boot.h" "main_boot")

(define version (let-values ([(major minor bug) (scheme-version-number)])
                    (string-append (number->string major) "." (number->string minor) "." (number->string bug))))

(define m-type (symbol->string (machine-type)))

(define lib-dir
    (find
        file-exists?
        (list
            (format "/opt/homebrew/Cellar/chezscheme/~a/lib/csv~a/~a" version version m-type)
            (format "/usr/lib/csv~a/~a" version m-type)
            (format "/usr/local/lib/csv~a/~a" version m-type))))

(unless lib-dir
    (display "could not find scheme installation") (newline)
    (exit 1))

(file-exists? (format "~a/~a.boot" lib-dir "scheme"))

(unless (file-exists? "scheme_boot.h")
    (boot->cheader (format "~a/scheme.boot" lib-dir) "scheme_boot.h" "scheme_boot"))

(unless (file-exists? "petite_boot.h")
    (boot->cheader (format "~a/petite.boot" lib-dir) "petite_boot.h" "petite_boot"))

(define os
    (cond
      ((and (fx>= (string-length m-type) 3)
            (string=? "osx" (substring m-type
                                (fx- (string-length m-type) 3)
                                (string-length m-type)))) 'osx)
      ((and (fx>= (string-length m-type) 2)
            (member (substring m-type
                        (fx- (string-length m-type) 2)
                        (string-length m-type))
                    '("ob" "nb" "fb"))) 'bsd)
      (else 'linux)))

(let ([gcc-libs (case os
                    ['osx "-L/opt/homebrew/lib -lz -llz4 -liconv -lncurses -lpthread -ldl -lm -framework CoreFoundation -framework CoreServices"]
                    ['bsd "-L/usr/local/lib -lz -llz4 -liconv -lncurses -lpthread -lm"]
                    ['linux "-lz -llz4 -liconv -lncurses -lpthread -ldl -lm -lrt"]
                    [else (display "unsupported os") (newline) (exit 1)])])
          (system
              (format "gcc -O3 main.c -rdynamic ~a/libkernel.a -I~a ~a -o buged"
                      lib-dir lib-dir gcc-libs)))