#lang racket/base

(provide (all-defined-out))
(require "./paths.rkt")

; This is how we deliver Racket code generated using the Vulkan spec.
; This procedure is kept seperate for the sake of a unit test.
(define (write-sequence registry make-sequence [out (current-output-port)])
  (for ([declaration (make-sequence registry)])
    (writeln declaration out)))


(define (write-racket-module-file! path registry make-sequence)
  (call-with-output-file #:exists 'replace
    path
    (λ (port)
      (displayln "#lang racket/base" port) ; Limits load time
      (write-sequence registry make-sequence port))))

; For maintainer use only.
(define (write-package-module-file! registry make-sequence . path-elements)
  (write-racket-module-file! (apply build-path package-path path-elements)
                             registry
                             make-sequence))

;; Run this module as a script to run a built-in generator for Racket
;; code. You can use this to piece together your own Racket module or
;; to verify if a section of code reflects Vulkan's specification.
(module+ main
  (require racket/cmdline
           racket/list
           racket/path
           racket/string
           "../spec.rkt")

  (define (get-generate-path path-el)
    (build-path private-path "generate" path-el))

  ; Compute valid arguments for usage help
  (define valid-names
    (filter-map
     (λ (f)
       (with-handlers ([exn? (λ _ #f)])
         (and (procedure? (dynamic-require (get-generate-path f)
                                           'generate-fragment))
              (string-replace (path->string f)
                              ".rkt"
                              ""))))
     (filter (λ (f) (equal? (path-get-extension f)
                            #".rkt"))
             (directory-list (build-path private-path
                                         "generate")))))

  (define (show-help)
    (displayln "Expected one argument naming the code you wish to generate.")
    (displayln (string-join (cons "The following values are accepted:"
                                  valid-names)
                            "\n")))

  (when (= (vector-length (current-command-line-arguments)) 0)
    (show-help)
    (exit 1))

  (define modname (command-line #:args (modname) modname))
  (unless (member modname valid-names)
    (show-help)
    (exit 1))

  (define modpath (path-replace-extension
                   (get-generate-path modname)
                   #".rkt"))

  (write-sequence (get-vulkan-spec 'local)
                  (dynamic-require modpath 'generate-fragment)))
