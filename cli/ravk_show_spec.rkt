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
         "../private/txexpr.rkt"
         "../private/generate/shared.rkt"
         "../spec.rkt"
         "./shared.rkt")

(define program-name (get-program-name (this-expression-file-name)))
(define-runtime-path cli-directory ".")

(define (process-command-line)
  (define show-version (make-parameter #f))
  (define show-xexpr (make-parameter #f))
  (command-line #:program program-name
                #:once-each
                [("-x" "--xexpr")
                 ("Print the output as an X-expression."
                  "Ignored if -v is set.")
                 (show-xexpr #t)]
                [("-v" "--version")
                 "Show the spec version instead of its contents."
                 (show-version #t)])

  (define source (get-source/dynamic))

  (define (extract-header-version registry)
    (define (header-version-define? x)
      (and (tag=? 'type x)
           (equal? (attr-ref x 'category "") "define")
           (equal? (get-type-name x) "VK_HEADER_VERSION")))
    (define text (shrink-wrap-cdata (findf-txexpr registry header-version-define?)))
    (regexp-replace* #px"(?m:\\D+)" text ""))

  (define (extract-spec-version registry)
    (define features (findf*-txexpr registry
                                    (λ (x) (and (tag=? 'feature x)
                                                (attrs-have-key? x 'number)))))
    (define number-attrs (map (λ (x) (attr-ref x 'number)) features))
    (define sorted (sort number-attrs (λ (a b) (> (string->number a) (string->number b)))))
    (car sorted))

  (define (display-version)
    (define registry (get-vulkan-spec source))
    (displayln (string-append (extract-spec-version registry)
                              "."
                              (extract-header-version registry))))

  (if (show-version)
      (display-version)
      (if (show-xexpr)
          (writeln (get-vulkan-spec source))
          (copy-port (get-spec-port source)
                     (current-output-port)))))
