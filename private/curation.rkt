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
         "./c-analysis.rkt")

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

(define (remove-category cat)
  (λ (types)
    (filter (λ (t) (not (equal? (attr-ref t 'category "") cat)))
            types)))


;; Categorizes <type requires="vk_platform"> as "ctype".
;; Interestingly, they are not already of category "basetype".
(define (categorize-c-types types)
  ; Don't include types that ffi/unsafe already provides.
  (define (already-in-racket? x)
    (member (cname (get-type-name x))
            '(_void _float _double _int)))

  (map (λ (x)
         (define requires (attr-ref x 'requires ""))
         (if (and (equal? requires "vk_platform")
                  (not (already-in-racket? x)))
             (attr-set x 'category "ctype")
             x))
       types))

(define get-type-lookup
  (simple-memo
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


#;(module+ test
  (require racket/set
           "../vulkan-spec.rkt")
  (test-case "(sort-types)"
    (define registry (get-vulkan-spec 'local))
    (define sorted (sort-types (get-tagged-children (find-first-by-tag 'types registry))))

    (for/fold ([encountered (set)])
              ([type (in-list sorted)])

      (set-add encountered (get-type-name type)))))


;; Not all <enums> elements are actually C enumerations.
;; Categorize them so we know to treat them differently.
(define (categorize-enums-that-arent enums-list)
  (filter-map
   (λ (x)
     (and (not (attrs-have-key? x 'type))
          (attr-set x 'category "consts")))
   enums-list))

(module+ test
  (test-equal? "(categorize-enums-that-arent)"
               (categorize-enums-that-arent
                '((enums ((name "n") (type "t"))
                         (enum ((name "a") (value "1")))
                         (enum ((name "b") (value "2"))))
                  (enums ((name "p"))
                         (enum ((name "x") (value "1")))
                         (enum ((name "y") (value "2"))))))
               '((enums ((category "consts") (name "p"))
                        (enum ((name "x") (value "1")))
                        (enum ((name "y") (value "2")))))))

;; Just toss a category on <command> elements for consistency.
;; Keeps the logic in generate-bindings.rkt easier to think about.
(define (categorize-commands commands-list)
  (filter-map (λ (x)
                (and (tag=? 'command x)
                     (attr-set x 'category "command")))
              commands-list))

; Return declaration elements in sorted groups.
(define (curate-registry registry)
  (define curate-types (compose sort-types
                                remove-c-macros
                                (remove-category "include")
                                (remove-category "")
                                categorize-c-types))

  (define curate-enums (compose categorize-enums-that-arent))
  (define curate-commands (compose categorize-commands))

  (define curated-declarations
    (append (curate-types (get-tagged-children (find-first-by-tag 'types registry)))
            (curate-enums (find-all-by-tag 'enums registry))
            (curate-commands (get-tagged-children (find-first-by-tag 'commands registry)))))

  ;; We want the basic types to always come first.
  (define forced-preamble-categories '("ctype" "basetype" "symdecl" "consts"))
  (define-values (basetypes customtypes)
    (partition (λ (x) (member (attr-ref x 'category)
                              forced-preamble-categories))
               curated-declarations))

  (append (filter (λ (x) (category=? "ctype" x)) basetypes)
          (filter (λ (x) (category=? "basetype" x)) basetypes)
          (filter (λ (x) (category=? "symdecl" x)) basetypes)
          (filter (λ (x) (category=? "consts" x)) basetypes)
          customtypes))
