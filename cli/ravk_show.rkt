#lang racket/base

(provide process-command-line summary)

(define summary "Review key information")

; Remove to prevent direct execution.
(module+ main (void (process-command-line)))

; Do not change anything below if you only want subcommands.
(require racket/cmdline
         racket/runtime-path
         (only-in mzlib/etc this-expression-file-name)
         natural-cli)

(define program-name (get-program-name (this-expression-file-name)))
(define-runtime-path cli-directory ".")

(define (process-command-line)
  (define-values (finish-expr arg-strings-expr help-expr unknown-expr)
    (make-subcommand-handlers cli-directory program-name))
  (command-line #:program program-name
                #:handlers finish-expr arg-strings-expr help-expr unknown-expr))
