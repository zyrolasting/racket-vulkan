#lang racket/base

(provide (all-defined-out))
(require racket/hash
         racket/list
         racket/set
         racket/string
         graph
         "./shared.rkt")

(define (in-fragment registry)
  (in-generator
    (define ordered (curate-registry registry))
    (define lookup (get-type-lookup ordered))

    ; To be clear, this is a superset of the category attribute values
    ; you'd expect to find in the Vulkan registry. (curate-registry)
    ; introduced a few of its own, and they are not restricted to
    ; <type> elements.
    (define category=>proc
      `#hash(("enum"         . ,generate-enum-signature)
             ("bitmask"      . ,generate-bitmask-signature)
             ("funcpointer"  . ,generate-funcpointer-signature)
             ("struct"       . ,generate-struct-signature)
             ("union"        . ,generate-union-signature)
             ("command"      . ,generate-command-signature)))

    (for ([type (in-list ordered)])
      (define category (attr-ref type 'category ""))
      (define alias (attr-ref type 'alias #f))
      (define make-datum (hash-ref category=>proc category #f))
      (when make-datum
        (yield (if alias
                   (let ([namer (if (tag=? 'command type) string->symbol cname)])
                     `(define ,(namer (get-type-name type)) ,(namer alias)))
                   (make-datum type registry lookup)))))))

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

; Return declaration elements in sorted groups.
(define (curate-registry registry)
  (define curated-declarations
    (append (sort-types (get-tagged-children (find-first-by-tag 'types registry)))
            (categorize-commands (get-tagged-children (find-first-by-tag 'commands registry)))))

  (define interdependent-categories '("struct" "union" "funcpointer" "command" "bitmask" "enum"))
  (filter (λ (x) (member (attr-ref x 'category "")
                         interdependent-categories))
          curated-declarations))


;; ------------------------------------------------
;; C unions correspond to <type category="union">

(define (generate-member-signature/union member-xexpr)
  (define undecorated-type (snatch-cdata 'type member-xexpr))
  (define cdata (get-all-cdata member-xexpr))
  (define characters (string->list cdata))
  (define array-size-match (regexp-match #px"\\[([^\\]]+)\\]" cdata))
  (define ctype (cname undecorated-type))
  (if array-size-match
      `(_list-struct . ,(build-list (string->number (cadr array-size-match))
                                    (λ _ ctype)))
      `(_list-struct ,ctype)))

(define (generate-union-signature union-xexpr [registry #f] [lookup #hash()])
  (define members (get-elements-of-tag 'member union-xexpr))
  (define name (get-type-name union-xexpr))
  `(begin
     (define ,(cname name)
       (_union
        . ,(map generate-member-signature/union
                members)))
     . ,(map
         (λ (x ordinal)
           `(define (,(string->symbol (string-append name
                                                     "-"
                                                     (shrink-wrap-cdata (find-first-by-tag 'name x)))) u)
              (union-ref u ,ordinal)))
         members
         (range (length members)))))

;; ------------------------------------------------
;; C structs correspond to <type category="struct">

(define (generate-struct-signature struct-xexpr [registry #f] [lookup #hash()])
  (define struct-name (get-type-name struct-xexpr))
  (define (generate-member-signature member-xexpr)
    (define name (snatch-cdata 'name member-xexpr))
    (define enum (find-first-by-tag 'enum member-xexpr))
    (define numeric-length (regexp-match #px"\\[(\\d+)\\]" (shrink-wrap-cdata member-xexpr)))
    (define undecorated-type (snatch-cdata 'type member-xexpr))
    (define characters (string->list (shrink-wrap-cdata member-xexpr)))
    (define inferred-type (infer-type (if (equal? undecorated-type struct-name)
                                                  "void"
                                                  undecorated-type)
                                              characters
                                              lookup))

    (define type (if enum
                     `(_array ,inferred-type ,(string->symbol (shrink-wrap-cdata enum)))
                     (if numeric-length
                         `(_array ,inferred-type ,(string->number (cadr numeric-length)))
                         inferred-type)))

    `(,(string->symbol name) ,type))

  `(define-cstruct ,(cname struct-name)
     . (,(map generate-member-signature
             (get-elements-of-tag 'member
                                  struct-xexpr)))))


;; ------------------------------------------------------------------
;; Enumerations are related by <type> and <enums> elements. Some
;; <enums> elements are a list of #defines. Others are actual C enums.
;; This is the case that generates actual C enums.
(define collect-extensions-by-enum-name
  (memoizer (λ (registry)
                 (foldl (λ (extension result)
                          (foldl (λ (enumerant collecting-enumerants)
                                   (hash-set collecting-enumerants
                                             (attr-ref enumerant 'name)
                                             extension))
                                 result
                                 (or (find-all-by-tag 'enum extension)
                                     '())))
                        #hash()
                        (find-all-by-tag 'extension registry)))))


(define (collect-enumerants-by-name where)
  (foldl (λ (enum result)
           (if (attrs-have-key? enum 'name)
               (hash-set result (attr-ref enum 'name) enum)
               result))
         #hash()
         (find-all-by-tag 'enum where)))

(define collect-enumerants-by-name/all
  (memoizer collect-enumerants-by-name))
(define collect-enumerants-by-name/core
  (memoizer (λ (registry)
                 (collect-enumerants-by-name
                  (find-first-by-tag 'enums registry)))))
(define collect-enumerants-by-name/features
  (memoizer (λ (registry)
                 (define hashes (map collect-enumerants-by-name (find-all-by-tag 'feature registry)))
                 (if (empty? hashes)
                     #hash()
                     (apply hash-union hashes)))))

(define collect-enumerant-name-counts
  (memoizer (λ (registry)
                 (foldl (λ (enum result)
                          (if (attrs-have-key? enum 'name)
                              (hash-set result
                                        (attr-ref enum 'name)
                                        (add1 (hash-ref result (attr-ref enum 'name) 0)))
                              result))
                        #hash()
                        (find-all-by-tag 'enum registry)))))

(define collect-enumerant-relationships
  (memoizer
   (λ (registry)
     (foldl (λ (x res)
              (hash-set res
                        (attr-ref x 'extends)
                        (cons x (hash-ref res (attr-ref x 'extends) '()))))
            #hash()
            (or (findf*-txexpr
                 registry
                 (λ (x) (and (tag=? 'enum x)
                             (attrs-have-key? x 'extends))))
                '())))))


(define (generate-enum-signature enum-xexpr registry [lookup #hash()])
  (define name (get-type-name enum-xexpr))
  (define extension-lookup (collect-extensions-by-enum-name registry))
  (define enum-lookup (collect-enumerants-by-name/all registry))
  (define enum-lookup/core (collect-enumerants-by-name/core registry))
  (define enum-lookup/features (collect-enumerants-by-name/features registry))
  (define relationship-lookup (collect-enumerant-relationships registry))
  (define name-counts (collect-enumerant-name-counts registry))

  (define (belongs-to-extension? name)
    (hash-has-key? extension-lookup name))

  ; Some enumerants have values computed in terms of enum ranges in other extensions.
  ; The spec covers how to compute these values.
  ; https://www.khronos.org/registry/vulkan/specs/1.1/styleguide.html#_assigning_extension_token_values
  (define (find-extension-relative-value enumerant)
    ; In English: First try to get the "extnumber" attribute value on
    ; the enumerant. Failing that, find the <extension> element that
    ; has the enumerant as a descendent and grab its "number"
    ; attribute value
    (define ext-number
      (string->number
       (attr-ref enumerant 'extnumber
                 (λ _ (attr-ref
                       (hash-ref extension-lookup (attr-ref enumerant 'name))
                       'number)))))

    (define base-value 1000000000)
    (define range-size 1000)
    (define offset (string->number (attr-ref enumerant 'offset)))
    (+ base-value (* (- ext-number 1) range-size) offset))

  ; Empty enums are possible.
  ; https://github.com/KhronosGroup/Vulkan-Docs/issues/1060
  (define enum-decl (hash-ref (collect-named-enums registry)
                              name
                              (λ _ '(enums))))

  ; Some enumerants are an alias for another enumerant.
  (define (resolve-alias enumerant)
    (define alias (attr-ref enumerant 'alias))
    (attr-set
     (hash-ref enum-lookup alias)
     'name
     (attr-ref enumerant 'name)))

  ; Pull out the intended (assumed numerical) value
  ; from the enumerant.
  (define (extract-value enumerant)
    (if (attrs-have-key? enumerant 'alias)
        (extract-value (resolve-alias enumerant))
        (if (attrs-have-key? enumerant 'offset)
            (find-extension-relative-value enumerant)
            (let ([n (if (attrs-have-key? enumerant 'bitpos)
                         (arithmetic-shift 1 (string->number (attr-ref enumerant 'bitpos)))
                         (let ([val (attr-ref enumerant 'value)])
                           (if (string-prefix? val "0x")
                               (string->number (string-replace val "0x" "") 16)
                               (string->number val))))])
              (if (equal? "-" (attr-ref enumerant 'dir #f))
                  (* -1 n)
                  n)))))

  ; Find the enumerants that extend this type.
  (define extensions
    (hash-ref relationship-lookup
              name
              (λ _ '())))

  ; HACK: For now, ignore the extension enums that duplicate definitions.
  (define deduped
    (filter
     (λ (x)
       (<= (hash-ref name-counts (attr-ref x 'name) 0)
           1))
     extensions))

  (define enumerants (append
                      (filter (λ (x) (tag=? 'enum x))
                              (get-elements enum-decl))
                      deduped))

  ; Pair up enumerant names and values.
  (define pairs (reverse (map (λ (x) (cons (attr-ref x 'name)
                                           (extract-value x)))
                              enumerants)))

  ; To be nice to Racketeers, let's give them easy flags when
  ; using Vulkan so they don't have to OR things together themselves.
  (define ctype (if (equal? "bitmask" (attr-ref enum-decl 'type ""))
                    '_bitmask
                    '_enum))

  ; _enum or _bitmask need a basetype to match how the values are used.
  ; https://docs.racket-lang.org/foreign/Enumerations_and_Masks.html?q=_enum#%28def._%28%28lib._ffi%2Funsafe..rkt%29.__enum%29%29
  (define basetype
    (if (equal? ctype '_enum)
        (if (ormap (λ (pair) (< (cdr pair) 0)) pairs)
            '_fixint
            '_ufixint)
        '_uint))

  `(begin
     (define ,(cname name)
       (,ctype
        ',(for/fold ([decls '()])
                    ([enumerant (in-list pairs)])
            ; The ctype declaration assumes a list of form (name0 = val0 name1 = val1 ...)
            (define w/value (cons (cdr enumerant) decls))
            (define w/= (cons '= w/value))
            (define w/all (cons (string->symbol (car enumerant)) w/=))
            w/all)
        ,basetype))
     . ,(for/list ([enumerant (in-list (reverse pairs))])
          `(define ,(string->symbol (car enumerant)) ,(cdr enumerant)))))


;; ------------------------------------------------------------------
; <type category="bitmask"> is just a C type declaration that happens
; to contain a typedef. Declaring _bitmask in Racket actually happens
; as part of processing enums.

(define (generate-bitmask-signature bitmask-xexpr [registry #f] [lookup #hash()])
  (define alias (attr-ref bitmask-xexpr 'alias #f))
  `(define ,(cname (get-type-name bitmask-xexpr))
     ,(cname (or alias
                 (snatch-cdata 'type
                           bitmask-xexpr
                           #:children-only? #t)))))


;; ------------------------------------------------------------------
;; <type category="funcpointer"> hurts a little because parameter
;; type tags are floating in a soup of C code. I assume that only
;; pointer indirection matters and check for '*' in the next sibling
;; strings after the parameter types. The return type is not even
;; in a tag at all, so I have a different approach to deduce it.

(define (generate-funcpointer-signature funcpointer-xexpr [registry #f] [lookup #hash()])
  (define name (get-type-name funcpointer-xexpr))
  (define text-signature (get-all-cdata funcpointer-xexpr))

  ; Deduce the formal parameter types
  (define children (get-elements funcpointer-xexpr))
  (define parameter-type-elements (filter (λ (x) (tag=? 'type x)) children))
  (define adjacent-cdata (map (λ (type-xexpr)
                                (list-ref children
                                          (add1 (index-of children type-xexpr))))
                              parameter-type-elements))

  (define parameter-types (map (λ (type-xexpr decl)
                                 (infer-type (shrink-wrap-cdata type-xexpr)
                                                     (string->list decl)
                                                     lookup))
                               parameter-type-elements
                               adjacent-cdata))

  ; Deduce the return type
  (define return-signature (cadr (regexp-match #px"typedef ([^\\(]+)" text-signature)))
  (define undecorated-return-type (regexp-replace* #px"[\\s\\*\\[\\]]" return-signature ""))
  (define return-type (infer-type undecorated-return-type
                                          (string->list return-signature)
                                          lookup))

  `(define ,(cname name)
     (_fun ,@parameter-types
           ->
           ,return-type)))


;; ------------------------------------------------------------------
;; All that stuff above was just the data. Now let's talk functions.


;; The return value of a function in context of the FFI is a bit tricky.
;; We want to capture pass-by-reference values returned to Racket, and
;; incorporate return code checking. This procedure generates code for
;; use as a `maybe-wrapper` in the `_fun` form. This assumes that `r`
;; is the identifier bound to the function's normal return value.
(define (generate-maybe-wrapper vkResult? who)
  (if (not vkResult?)
      null
      `(-> (check-vkResult r ',who))))


(define (generate-type-spec param)
  (define c-code (shrink-wrap-cdata param))
  (define ctype/text (get-text-in-tagged-child 'type param))
  (define pointer? (string-contains? c-code "*"))

  (if pointer?
      '_pointer
      (cname ctype/text)))


(define (generate-command-signature command-xexpr [registry #f] [lookup #hash()])
  (define children (filter (λ (x) (and (txexpr? x)
                                       (member (get-tag x) '(param proto))))
                           (get-elements command-xexpr)))

  ; <proto> always comes first.
  ; https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_contents_of_command_tags
  (define proto (car children))
  (define id (string->symbol (get-text-in-tagged-child 'name proto)))
  (define undecorated-return (get-text-in-tagged-child 'type proto))
  (define characters (string->list (shrink-wrap-cdata proto)))
  (define ret (infer-type undecorated-return
                                  characters
                                  lookup))

  (define param-elements (cdr children))
  (define type-specs (map generate-type-spec param-elements))
  (define auto-check-return-code? (and (equal? undecorated-return "VkResult")
                                       (enable-auto-check-vkresult)))

  `(define-vulkan ,id
     (_fun ,@type-specs
           ->
           ,(if (equal? ret '_void)
                ret
                `(r : ,ret))
           . ,(generate-maybe-wrapper auto-check-return-code?
                                      id))))
