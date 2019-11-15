#lang racket/base

(provide process-command-line summary)

(define summary "Show unstable generator module directory.")

; Remove to prevent direct execution.
(module+ main (void (process-command-line)))

(require racket/cmdline
         racket/runtime-path
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "../private/paths.rkt")

(define program-name (get-program-name (this-expression-file-name)))
(define-runtime-path cli-directory ".")

(define (process-command-line)
  (define-values (finish-expr arg-strings-expr help-expr unknown-expr)
    (make-subcommand-handlers cli-directory program-name))
  (command-line #:program program-name
                #:args _
                (displayln (simplify-path (build-path private-path "generate")))))
