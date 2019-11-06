#lang racket/base

(provide (all-defined-out)
         (rename-out [generate-api-constant-declarations generate-fragment]))
(require racket/generator
         racket/list
         racket/string
         "./shared.rkt"
         "../analyze/spec.rkt"
         "../analyze/c.rkt"
         "../analyze/txexpr.rkt")


;; Declare all Vulkan API constants as Racket code.
(define (generate-api-constant-declarations registry)
  (in-generator
   (yield (generate-define-constant-signature registry "VK_HEADER_VERSION"))
   (yield (generate-define-constant-signature registry "VK_NULL_HANDLE"))
   (yield* (generate-consts-from-enums-elements registry))))


;; Input:  <type>#define <name>X</name> 10</type>
;; Output: (define X 10)
;; Assumes the value is always a decimal number.
(define (generate-define-constant-signature registry target-name)
  `(define
     ,(string->symbol target-name)
     ,(string->number
       (regexp-replace*
        #px"\\D+"
        (shrink-wrap-cdata
         (findf-txexpr (findf-txexpr registry
                                     (λ (x) (tag=? 'types x)))
                       (λ (x)
                         (and (tag=? 'type x)
                              (equal? (get-type-name x)
                                      target-name)))))
        ""))))


;; Turns <enums> representing Vulkan API constants into Racket code.
;; The predicate derived from https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#tag-enums
;; says that <enums> elements without a `type` attribute represent
;; API constants. But those don't include extension constants,
;; so we have to grab those separately.
(define (generate-consts-from-enums-elements registry)
  (in-generator
   (for ([enums-element (get-all-enums-elements registry)])
     (unless (attrs-have-key? enums-element 'type)
       (yield* (generate-consts-signature enums-element))))))

(define (get-all-enums-elements registry)
  (append (find-all-by-tag 'enums registry)
          (list (find-extension-constants registry))))

;; Vulkan extensions can declare their own constants.
;; Yank them out as <enums> elements so we can process
;; them as such.
(define (find-extension-constants registry)
  (define (accum source fetch)
    (foldl (λ (x res)
             (define matches (fetch x))
             (if matches
                 (append res matches)
                 res))
           '()
           source))

  ; The enumerants are inside <require> elements.
  (define require-elements
    (accum
     (find-all-by-tag 'extension registry)
     (λ (x) (find-all-by-tag 'require x))))
  (define enum-elements
    (accum
     require-elements
     (λ (x) (find-all-by-tag 'enum x))))

  ; Enumerants that don't extend a C enum type are API constants.
  `(enums . ,(filter (λ (x) (and (attrs-have-key? x 'value)
                        (not (attrs-have-key? x 'extends))))
                     enum-elements)))


;; Convert an <enums> element representing API constants into Racket code.
(define (generate-consts-signature enums-xexpr)
  (in-generator
   (define enumerant-list (filter (λ (x) (tag=? 'enum x))
                                  (get-elements enums-xexpr)))
   (for ([enumerant enumerant-list])
     (yield `(define ,(string->symbol (attr-ref enumerant 'name))
               ,(if (attrs-have-key? enumerant 'alias)
                    (string->symbol (attr-ref enumerant 'alias))
                    (extract-value enumerant)))))))

;; Creates a Racket value from a Vulkan <enum> element representing
;; an API constant.
(define (extract-value enumerant)
  (define value (attr-ref enumerant 'value))
  (if (string-contains? value "\"")
      (string->bytes/utf-8 value)
      (let ([num-expr (c-numeric-lit->number value)])
        (if (equal? (attr-ref enumerant 'dir #f) "-1")
            `(* -1 ,num-expr)
            num-expr))))


;; A limited converter from C numeric literals to a Racket number.
;;
;; Assumptions:
;; 1. If "~" is in the string, it is a complement of 0.
;; 2. All other values equals the decimal value of the first
;;    contiguous sequence of digits in the string.
(define (c-numeric-lit->number c-num-lit-string)
  (if (string-contains? c-num-lit-string "~")
      (compute-~0-declaration c-num-lit-string)
      (string->number (car (regexp-match* #px"\\d+"
                                          c-num-lit-string)))))


;; This procedure is a tiny, limited parser for a subset of C numeric
;; literals. Namely, complements of 0. The returned quasiquoted value
;; expands such that (ctype-sizeof) runs on the client's system, but
;; the "LL" check expands while control is in this procedure. This is
;; to make sure the client's system uses its own word size.
(define (compute-~0-declaration literal)
  ; Extract subtraction operand
  (define sub-op/match (regexp-match* #px"-\\d" literal))
  (define sub-op (if (empty? sub-op/match)
                     0
                     (abs (string->number (car sub-op/match)))))

  `(- (integer-bytes->integer
       (make-bytes
        (ctype-sizeof ,(if (string-contains? literal "LL") '_llong '_long))
        255)
       ,(string-contains? literal "U"))
      ,sub-op))
