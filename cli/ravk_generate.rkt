#lang racket/base

(provide process-command-line summary)

(define summary "Write code fragment from module to STDOUT.")

; Do not change anything below if you only want subcommands.
(require racket/cmdline
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "./shared.rkt"
         "../private/writer.rkt")

(define program-name (get-program-name (this-expression-file-name)))

(define (process-command-line)
  (command-line #:program program-name
                #:args module-paths
                (for ([module-path module-paths])
                  (write-generated module-path))))
