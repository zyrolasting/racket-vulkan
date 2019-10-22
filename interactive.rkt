#lang racket/base

;; Use this module in a REPL with `,enter`, or in DrRacket.  It's
;; useful for running queries on vk.xml to inform decisions about the
;; project. It is not meant for use in the generation program
;; itself. Place any procedures that WOULD be useful for that in
;; another module.

(require racket/set
         "./private/txexpr.rkt"
         "./vulkan-spec.rkt")

;; The interface is loosely inspired by a browser console API, in
;; which '$$' is like 'querySelectorAll' and '$' is like
;; 'querySelector' (Except the selectors can only be for individual
;; tags).

(define registry (get-vulkan-spec 'local))
(define ($$ t [tx registry]) (find-all-by-tag t tx))
(define ($ t [tx registry]) (find-first-by-tag t tx))
(define txt shrink-wrap-cdata)

(define (possible-return-values)
  (set->list (apply set (map (λ (x) (txt ($ 'type x)))
                             ($$ 'proto)))))
