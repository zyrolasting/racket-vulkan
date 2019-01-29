#lang racket/base

;---------------------------------------------------------------------------------------------------
; This module generates FFI bindings for Vulkan

(require
  "./vulkan-spec.rkt" ; For sourcing VulkanAPI spec
  graph               ; For mapping type dependencies
  racket/contract     ; For module contracts
  racket/match        ; For xexpr processing
  xml)                ; For contract predicates

(provide
  (contract-out
    ; Return a list of datums that can be written as a Racket module.
    [generate-vulkan-bindings (-> vulkan-spec? list?)]))

; Run this script directly to display the generated Racket module on (current-output-port)
(module+ main
  (for ([i (generate-vulkan-bindings (get-vulkan-spec 'local))])
    (displayln i)))


;---------------------------------------------------------------------------------------------------
; Implementation

; These functions match element types defined in the Vulkan registry
; and are invoked for each associated element in the order encountered.
; Registry guide: https://www.khronos.org/registry/vulkan/specs/1.1/registry.html
(define (process-registry attrs children)
  (for/list ([x children])
    (process-xexpr! x)))

(define (get-attr-value sym attrs)
  (let ([res (assoc sym attrs)])
    (if res (list-ref res 1) #f)))

(define (process-comment attrs children)
  '())

(define (process-platforms attrs children)
  '())

(define (process-tags attrs children)
  '())

(define (process-types attrs children)
  '())

; Forward type declaration to dependency graph
(define (process-type attrs children)
  (define name (get-attr-value 'name attrs))
  (define requires (get-attr-value 'requires attrs))
  (if name (add-vertex! type-dependencies name) (void))
  (if requires
    (begin
      (add-vertex! type-dependencies name)
      (add-directed-edge! type-dependencies name requires))
    (void))

(define (process-enums attrs children)
  '())

(define (process-enum attrs children)
  '())

(define (process-unused attrs children)
  '())

(define (process-commands attrs children)
  '())

(define (process-feature attrs children)
  '())

(define (process-extensions attrs children)
  '())

(define (process-cdata s)
  '())

(define (process-string s)
  '())

(define (process-element x)
  (let [(tag (car x)) (attrs (car (cdr x))) (children (cddr x))]
    (define proc (select-element-procedure tag))
    (proc attrs children)))

; Look up a procedure based on tag element. Will fail
; if specification tag names disagree.
(define ns (variable-reference->namespace (#%variable-reference)))
(define (select-element-procedure s)
  (define sym (string->symbol (string-append "process-" (symbol->string s))))
  (eval sym ns))

; Patterns adapted from xexpr?
; https://docs.racket-lang.org/xml/index.html#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29
(define (process-xexpr! x)
  (match x
    [(list t (list (list an av) ...) c ...) (process-element x)]
    [(cons t (list c ...)) (process-element x)]
    [(struct* cdata ([string s])) (process-cdata s)]
    [(regexp #rx".*") (process-string x)]
    [_ (error "Unaccounted")]))

(define (generate-vulkan-bindings xexpr)
  (parameterize ([type-dependencies (make-directed-graph '())])
    (process-xexpr! xexpr)
    (define type-sequence (tsort (transpose type-dependencies)))
    ))
