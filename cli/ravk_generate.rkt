#lang racket/base

(provide process-command-line summary)

(define summary "Write code fragment from module to STDOUT.")

; Do not change anything below if you only want subcommands.
(require racket/cmdline
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "./shared.rkt"
         "../private/paths.rkt"
         "../private/generate/shared.rkt"
         "../private/writer.rkt")

(define program-name (get-program-name (this-expression-file-name)))

(define names=>modules
  #hash(("unsafe" . "make-unsafe.rkt")))
(define (resolve name)
  (if (and (hash-has-key? names=>modules name) (not (file-exists? name)))
      (build-path private-path "generate" (hash-ref names=>modules name))
      name))

(define (process-command-line)
  (define config (make-hash))
  (command-line #:program program-name
                #:once-each
                [("--enable-auto-check-vkresult")
                 ("When set, foreign function wrappers will automatically "
                  "check VkResult values and raise exceptions for error codes.")
                 (hash-set! config 'enable-auto-check-vkresult #t)]
                [("--enable-symbolic-enums")
                 ("When set, C enum types are represented using _enum and _bitmask"
                  "types in your bindings.")
                 (hash-set! config 'enable-symbolic-enums #t)]
                #:args (path-or-name . others) ; Force at least one
                (for ([target (cons path-or-name others)])
                  (define module-path (resolve target))
                  (write-generated module-path config))))
