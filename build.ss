#!/opt/homebrew/bin/chez --script

(optimize-level 3)
(debug-level 0)
(compile-imported-libraries #t)
(generate-wpo-files #t)
(compile-program "main.ss")
(compile-whole-program "main.wpo" "main.so")

(fasl-compressed #f)
(make-boot-file "main.boot" '("petite") "main.so")
(vfasl-convert-file "main.boot" "main.boot" '("petite"))
