#lang racket/base

(provide process-command-line summary)

(define summary "Controls for Vulkan-Racket projects")

(module+ main (void (process-command-line)))

(require racket/cmdline
         racket/runtime-path
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "./shared.rkt")

(define program-name (get-program-name (this-expression-file-name)))
(define-runtime-path cli-directory ".")

(define (process-command-line)
  (define-values (finish-expr arg-strings-expr help-expr unknown-expr)
    (make-subcommand-handlers cli-directory program-name))
  (command-line #:program program-name
                #:once-each [("-l" "--latest")
                             ("Use the latest official vk.xml from the Khronos Group,"
                              "whenever relevant (Requires network connection).")
                             (use-latest? #t)]
                #:handlers finish-expr arg-strings-expr help-expr unknown-expr))
