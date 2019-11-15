#lang racket/base

(provide process-command-line summary)

(define summary "Write code fragment from module to STDOUT.")

; Do not change anything below if you only want subcommands.
(require racket/cmdline
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "./shared.rkt"
         "../private/generate/shared.rkt"
         "../private/writer.rkt")

(define program-name (get-program-name (this-expression-file-name)))

(define (process-command-line)
  (command-line #:program program-name
                #:once-each
                [("--enable-auto-check-vkresult")
                 ("When set, foreign function wrappers will automatically "
                  "check VkResult values and raise exceptions for error codes.")
                 (enable-auto-check-vkresult #t)]
                [("--enable-symbolic-enums")
                 ("When set, C enum types are represented using _enum and _bitmask"
                  "types in your bindings.")
                 (enable-symbolic-enums #t)]
                #:args module-paths
                (for ([module-path module-paths])
                  (write-generated module-path))))
