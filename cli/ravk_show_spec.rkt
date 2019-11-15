#lang racket/base

(provide process-command-line summary)

(define summary "Print the Vulkan specification.")

(module+ main (void (process-command-line)))

; Do not change anything below if you only want subcommands.
(require racket/cmdline
         racket/runtime-path
         racket/port
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "../spec.rkt"
         "./shared.rkt")

(define program-name (get-program-name (this-expression-file-name)))
(define-runtime-path cli-directory ".")

(define (process-command-line)
  (define show-latest (make-parameter #f))
  (define show-xexpr (make-parameter #f))
  (command-line #:program program-name
                #:once-each
                [("-x" "--xexpr")
                 "Print the output as an X-expression"
                 (show-xexpr #t)])

  (define source (if (use-latest?) 'remote 'local))
  (if (show-xexpr)
      (writeln (get-vulkan-spec source))
      (copy-port (get-spec-port source)
                 (current-output-port))))
