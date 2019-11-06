#lang racket/base

;; -------------------------------------------------------------------
;; This module makes the Vulkan Registry easier to process.

(provide (all-defined-out))

(require racket/function
         racket/list
         racket/set
         racket/string
         graph
         "./txexpr.rkt"
         "./c.rkt"
         "./memos.rkt")

(module+ test
  (require rackunit))

;; The "define" <type> category includes both C macros and C type
;; declarations. Strip out the macros to disambiguate the data. Note
;; that the filter predicate assumes all macros start with VK_ and are
;; all caps. Just checking for '#define' in the text will remove
;; legitimate declarations.
(define (remove-c-macros types)
  (filter (λ (t)
            (define name (get-type-name t))
            (define category (attr-ref t 'category ""))
            (define is-macro (and (equal? category "define")
                                  (string-prefix? name "VK_")
                                  (equal? (string-upcase name)
                                          name)))
            (not is-macro))
          types))

(define get-type-elements
  (memoizer (λ (registry)
              (get-tagged-children (find-first-by-tag 'types registry)))))

(define (remove-category cat)
  (λ (types)
    (filter (λ (t) (not (equal? (attr-ref t 'category "") cat)))
            types)))

(define get-type-lookup
  (memoizer
   (λ (types)
     (make-immutable-hash (map (λ (x) (cons (get-type-name x) x))
                               types)))))


;; Unfortunately the registry makes no guarentee that types will appear
;; in a specific order. If you simply blast the types out in the order
;; given to you in vk.xml, Racket will complain of unbound identifiers.
;;
;; That, and the `requires` attribute is not useful for modeling type
;; dependencies. The only real option is to build a directed graph of
;; name references and artificially discard recursive type references.
;; The result should be a DAG, but later parts of the code has to exercise
;; caution re: forward declarations and recursive definitions.
(define (sort-types types)
  ;; Build a lookup so we can work with type names alone.
  (define lookup (get-type-lookup types))
  (define (resolve name)
    (hash-ref lookup name))

  (define (get-custom-type-dependencies tx)
    (map (λ (x) (get-text-in-tagged-child 'type x))
         (find-all-by-tag 'member tx)))

  (define (get-funcpointer-dependencies tx)
    (map shrink-wrap-cdata
         (filter (λ (x) (tag=? 'type x))
                 (get-elements tx))))

  ;; Return a list of dependent types.
  (define (get-type-dependencies type-xexpr)
    (define dependent-name (get-type-name type-xexpr))
    (define alias (attr-ref type-xexpr 'alias #f))
    (define get-dependencies (case (attr-ref type-xexpr 'category "")
                               [("struct" "union") get-custom-type-dependencies]
                               [("funcpointer") get-funcpointer-dependencies]
                               [else (λ _ '())]))

    ; Exclude recursive type declarations and names that do not
    ; appear as declared types in the registry (Rare).
    (if alias
        (list alias)
        (filter-map
         (λ (dependency-name)
           (and (hash-has-key? lookup dependency-name)
                (not (equal? dependent-name dependency-name))
                dependency-name))
         (get-dependencies type-xexpr))))

  ;; Used for a fold that builds directed edges in the dependency graph.
  ;; I use a mock "ROOT" vertex to make sure elements with no dependencies
  ;; appear in the graph.
  (define (accumulate-dependencies xexpr edges)
    (define dependent-type (get-type-name xexpr))
    (append (map (λ (dependency-typename)
                   (list dependency-typename
                         dependent-type))
                 (cons "ROOT" (get-type-dependencies xexpr)))
            edges))

  ; Place the most dependent types last.
  (define most-to-least-responsible/names
    (remove "ROOT"
            (tsort (directed-graph
                    (foldl accumulate-dependencies '() types)))))

  ; Use lookup to transform names back to elements, and tack
  ; the aliased elements back on.
  (map resolve most-to-least-responsible/names))

;; Just toss a category on <command> elements for consistency.
;; Keeps the logic in generate-bindings.rkt easier to think about.
(define (categorize-commands commands-list)
  (filter-map (λ (x)
                (and (tag=? 'command x)
                     (attr-set x 'category "command")))
              commands-list))


; Type names appear in attribute or in CDATA of <name> element.
; https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_attributes_of_type_tags
(define (get-type-name type-element)
  (attr-ref type-element
            'name
            (λ _
              (define name-element
                (findf-txexpr type-element
                              (λ (x) (and (list? x)
                                          (equal? (get-tag x) 'name)))))
              (and name-element
                   (shrink-wrap-cdata name-element)))))

(define (type-name=? type-element name)
  (equal? (get-type-name type-element) name))

(define collect-enums
  (memoizer (λ (registry)
              (find-all-by-tag 'enums registry))))

(define collect-named-enums
  (memoizer (λ (registry)
              (foldl (λ (x h) (if (attrs-have-key? x 'name)
                                  (hash-set h (attr-ref x 'name) x)
                                  h))
                     #hash()
                     (collect-enums registry)))))

(define (get-type-by-category cat registry)
    (findf*-txexpr registry
                   (λ (x) (and (txexpr? x)
                               (equal? 'type (get-tag x))
                               (equal? cat (attr-ref x 'category #f))))))

; Return declaration elements in sorted groups.
(define (curate-registry registry)
  (define curate-types (compose sort-types
                                remove-c-macros
                                (remove-category "include")
                                (remove-category "")))

  (define curate-commands (compose categorize-commands))

  (define curated-declarations
    (append (curate-types (get-tagged-children (find-first-by-tag 'types registry)))
            (curate-commands (get-tagged-children (find-first-by-tag 'commands registry)))))

  ;; We want the basic types to always come first.
  (define forced-preamble-categories '("basetype" "symdecl" "consts"))
  (define-values (basetypes customtypes)
    (partition (λ (x) (member (attr-ref x 'category)
                              forced-preamble-categories))
               curated-declarations))

  customtypes)
