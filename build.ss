#!/opt/homebrew/bin/chez --script

(optimize-level 0)
(debug-level 3)
(compile-imported-libraries #t)
(generate-wpo-files #t)
(compile-program "main.ss")
(compile-whole-program "main.wpo" "main.so")

(fasl-compressed #f)
(make-boot-file "main.boot" '("petite") "main.so")
(vfasl-convert-file "main.boot" "main.boot" '("petite"))
