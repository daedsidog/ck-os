(defpackage #:ck-os/win32/gdi32
  (:use #:cl #:ck-clle #:cffi #:ck-os/win32/common)
  (:export #:create-solid-brush
           #:destroy-gdi-object
           #:fill-rectangle
           #:set-text-color
           #:set-text-transparent-bg
           #:set-text-bg-color))

(in-package #:ck-os/win32/gdi32)

(defvar *library-loaded* nil)

(defmacro libdefun (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (unless *library-loaded*
       (load-foreign-library "gdi32.dll")
       (setf *library-loaded* t))
     ,@body))

(libdefun window-device-context (window-handle)
  "Return the device context of the WINDOW-HANDLE."
  (foreign-funcall "GetDC"
                   :pointer window-handle
                   :pointer))

(defcenum device-capabilities-index
  (:driver-version                      0)
  (:technology                          2)
  (:horizontal-size                     4)
  (:vertical-size                       6)
  (:horizontal-resolution               8)
  (:vertical-resolution                10)
  (:bits-per-pixel                     12)
  (:planes                             14)
  (:number-of-brushes                  16)
  (:number-of-pens                     18)
  (:number-of-fonts                    22)
  (:number-of-colors                   24)
  (:device-size                        26)
  (:curve-capabilities                 28)
  (:line-capabilities                  30)
  (:polygonal-capabilities             32)
  (:text-capabilities                  34)
  (:clip-capabilities                  36)
  (:raster-capabilities                38)
  (:relative-width                     40)
  (:relative-height                    42)
  (:diagonal-width                     44)
  (:shade-blend-capabilities           45)
  (:horizontal-pixels-per-logical-inch 88)
  (:vertical-pixels-per-logical-inch   90)
  (:size-palette                       104)
  (:number-of-reserved                 106)
  (:color-resolution                   108)
  (:physical-width                     110)
  (:physical-height                    111)
  (:physical-horizontal-offset         112)
  (:physical-vertical-offset           113)
  (:horizontal-scaling-factor          114)
  (:vertical-scaling-factor            115)
  (:vertical-refresh                   116)
  (:desktop-vertical-resolution        117)
  (:desktop-horizontal-resolution      118)
  (:blt-alignment                      119))

(libdefun device-capabilities (device-context index)
  (foreign-funcall "GetDeviceCaps"
                   :pointer device-context
                   :int (foreign-enum-value 'device-capabilities-index
                                            index)
                   :int))

(libdefun create-solid-brush (color)
  (let ((brush (foreign-funcall "CreateSolidBrush"
                                :uint32 color
                                :pointer)))
    (when (eq brush #.(null-pointer))
      (error "Failed to create solid brush: WinAPI error ~A" (last-system-error)))
    brush))

(libdefun destroy-gdi-object (object)
  (let ((success (foreign-funcall "DeleteObject"
                                  :pointer object
                                  :bool)))
    (unless success
      (error "Failed to delete GDI object: WinAPI error ~A" (last-system-error)))
    success))

(libdefun fill-rectangle (device-context brush rect)
  (let ((success (foreign-funcall "FillRect"
                                  :pointer device-context
                                  :pointer (pointer-of rect)
                                  :pointer brush
                                  :bool)))
    (unless success
      (error "Failed to fill rectangle: WinAPI error ~A" (last-system-error)))
    success))

(defcenum color-references
  (:invalid-color #xffffffff))

(libdefun set-text-bg-color (device-context color)
  (let ((result (foreign-funcall "SetBkColor"
                                 :pointer device-context
                                 :uint32 color
                                 :pointer)))
    (when (eq result #.(null-pointer))
      (error "Failed to set background color: WinAPI error ~A" (last-system-error)))
    result))

(libdefun set-text-color (device-context color)
  (let ((result (foreign-funcall "SetTextColor"
                                 :pointer device-context
                                 :uint32 color
                                 :uint32)))
    (when (eq result (foreign-enum-value 'color-references :invalid-color))
      (error "Failed to set text color: WinAPI error ~A" (last-system-error)))
    result))

(defcenum text-background-modes
  (:transparent 1)
  (:opaque      2))

(libdefun set-text-transparent-bg (device-context transparent)
  (let ((result (foreign-funcall "SetBkMode"
                                 :pointer device-context
                                 :int (if transparent
                                          (foreign-enum-value 'text-background-modes :transparent)
                                          (foreign-enum-value 'text-background-modes :opaque))
                                 :int)))
    (when (zerop result)
      (error "Failed to set background mode: WinAPI error ~A" (last-system-error)))
    result))
