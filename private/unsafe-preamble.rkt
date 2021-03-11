#lang racket/base

(provide (all-defined-out))
(require ffi/unsafe ffi/unsafe/define)

(define libname (case (system-type 'os)
                  [(windows) "vulkan-1"]
                  [else "libvulkan"]))

(define-ffi-definer define-vulkan (ffi-lib libname)
  #:default-make-fail make-not-available)

(define _VisualID _ulong)
(define _Window _ulong)
(define _RROutput _ulong)
(define _Display 'Display)

; Wayland
(define _wl_display 'wl_display)
(define _wl_surface 'wl_surface)

; Windows
; http://web.archive.org/web/20190911051224/https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types
(define _HANDLE (_cpointer _void))
(define _HINSTANCE _HANDLE)
(define _HWND _HANDLE)
(define _HMONITOR _HANDLE)
(define _DWORD _ulong)
(define _LPCWSTR (_cpointer _wchar))
(define _SECURITY_ATTRIBUTES 'SECURITY_ATTRIBUTES)

; XCB
; https://code.woboq.org/qt5/include/xcb/xproto.h.html
(define _xcb_visualid_t _uint32)
(define _xcb_window_t _uint32)
(define _xcb_connection_t 'xcb_connection_t)

; Zircon (Fuchsia OS)
; https://fuchsia.googlesource.com/fuchsia/+/master/zircon/system/public/zircon/types.h
(define _zx_handle_t _uint32)

; These are apparently behind an NDA. Even if I knew what these were,
; I couldn't put them here.
; https://github.com/KhronosGroup/Vulkan-Docs/issues/1000
(define _GgpStreamDescriptor (_cpointer _void))
(define _GgpFrameToken (_cpointer _void))

(define (VK_MAKE_VERSION major minor patch)
  (bitwise-ior (arithmetic-shift major 22)
               (arithmetic-shift minor 12)
               (arithmetic-shift patch 0)))

(define VK_API_VERSION_1_0 (VK_MAKE_VERSION 1 0 0))
(define VK_API_VERSION_1_1 (VK_MAKE_VERSION 1 1 0))

(define (VK_VERSION_MAJOR v)
  (arithmetic-shift v -22))
(define (VK_VERSION_MINOR v)
  (bitwise-and (arithmetic-shift v -12) #x3ff))
(define (VK_VERSION_PATCH v)
  (bitwise-and v #xfff))

(define (format-vulkan-spec-version spec-v)
  (format "~a.~a.~a"
          (VK_VERSION_MAJOR spec-v)
          (VK_VERSION_MINOR spec-v)
          (VK_VERSION_PATCH spec-v)))
