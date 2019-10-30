#lang racket/base


;---------------------------------------------------------------------------------------------------
; This module offers the Vulkan API specification in machine-readable form.

(require xml
         racket/contract
         racket/runtime-path)

(provide
  (contract-out
    [get-vulkan-spec     (-> sources/c vulkan-spec?)] ; Computes Vulkan API spec
    [update-local-mirror (-> any/c)]                  ; Downloads stable Vulkan API spec to local mirror
    [vulkan-spec?        (-> any/c boolean?)]))       ; Returns if argument is a Vulkan specification according to this library

; Specification sources can be the local file system, or a remote system on the Internet
(define sources/c (symbols 'local 'remote))

; What 'local implies
(define-runtime-path registry-dir ".")
(define local-mirror-path (build-path registry-dir "vk.xml"))

; Run this script directly to see Vulkan spec xexpr on (current-output-port)
(module+ main (writeln (get-vulkan-spec)))


;---------------------------------------------------------------------------------------------------
; Implementation

(require net/url racket/port)

; TODO: Because xml does not (yet?) support DTD processing and
; RelaxNG is needed to generate declaration info, I'll ride
; on optimism for the time being.
(define (vulkan-spec? v)
  (and (xexpr? v) (eq? (car v) 'registry)))

; Downloads Vulkan API spec from official source.
; Multiple versions of the API may be generated off master.
(define/contract (source-spec-from-internet)
  (-> input-port?)
  (get-pure-port (string->url "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml")))

; Returns input port to a mirror of the Vulkan API spec from this package distribution.
(define/contract (source-spec-from-local-mirror)
  (-> input-port?)
  (open-input-file local-mirror-path))

; Writes network-sourced content to the local mirror.
(define (update-local-mirror)
  (call-with-output-file*
    local-mirror-path
    #:exists 'replace
    (lambda (port)
      (display (port->string (source-spec-from-internet)) port))))

; Returns an input port given a desired source
(define/contract (get-spec-port source)
  (-> sources/c input-port?)
  (if (eq? source 'local)
    (source-spec-from-local-mirror)
    (source-spec-from-internet)))

; Returns the Vulkan API specification
(define (get-vulkan-spec [spec-source 'local])
  (define p (get-spec-port spec-source))
  (define doc (read-xml p))
  (close-input-port p)
  (xml->xexpr (document-element doc)))

(module+ test
  (require rackunit)
  (check-pred xexpr? (get-vulkan-spec 'local)))
