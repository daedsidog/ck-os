(defpackage #:ck-os/win32/gdi32
  (:use #:cl #:ck-clle #:cffi #:ck-os/win32/common)
  (:export #:create-solid-brush
           #:delete-object
           #:fill-rectangle
           #:set-text-color
           #:set-text-transparent-bg
           #:set-text-bg-color
           #:create-compatible-dc
           #:create-compatible-bitmap
           #:select-object
           #:destroy-dc
           #:bit-blit))

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
  (:driverversion   0)
  (:technology      2)
  (:horzsize        4)
  (:vertsize        6)
  (:horzres         8)
  (:vertres         10)
  (:bitspixel       12)
  (:planes          14)
  (:numbrushes      16)
  (:numpens         18)
  (:numfonts        22)
  (:numcolors       24)
  (:pdevicesize     26)
  (:curvecaps       28)
  (:linecaps        30)
  (:polygonalcaps   32)
  (:textcaps        34)
  (:clipcaps        36)
  (:rastercaps      38)
  (:aspectx         40)
  (:aspecty         42)
  (:aspectxy        44)
  (:logpixelsx      88)
  (:logpixelsy      90)
  (:sizepalette     104)
  (:numreserved     106)
  (:colorres        108)
  (:physicalwidth   110)
  (:physicalheight  111)
  (:physicaloffsetx 112)
  (:physicaloffsety 113)
  (:scalingfactorx  114)
  (:scalingfactory  115)
  (:vrefresh        116)
  (:desktopvertres  117)
  (:desktophorzres  118)
  (:bltalignment    119)
  (:shadeblendcaps  120)
  (:colormgmtcaps   121))

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

(libdefun delete-object (object)
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
  (:clr-invalid #xffffffff))

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

(libdefun create-compatible-dc (hdc)
          (let ((memory-dc (foreign-funcall "CreateCompatibleDC"
                                            :pointer hdc
                                            :pointer)))
            (when (eq memory-dc #.(null-pointer))
              (error "Failed to create compatible DC: WinAPI error ~A" (last-system-error)))
            memory-dc))

(libdefun destroy-dc (hdc)
  (unless (zerop (foreign-funcall "DeleteDC" :pointer hdc :int))
    (error "Failed to delete DC: WinAPI error ~A" (last-system-error))))

(libdefun create-compatible-bitmap (hdc width height)
          (let ((bitmap (foreign-funcall "CreateCompatibleBitmap"
                                         :pointer hdc
                                         :int width
                                         :int height
                                         :pointer)))
            (when (eq bitmap #.(null-pointer))
              (error "Failed to create compatible bitmap: WinAPI error ~A" (last-system-error)))
            bitmap))

(libdefun select-object (hdc gdi-object)
          (let ((previous-object (foreign-funcall "SelectObject"
                                                  :pointer hdc
                                                  :pointer gdi-object
                                                  :pointer)))
            (when (eq previous-object #.(null-pointer))
              (error "Failed to select object into DC: WinAPI error ~A" (last-system-error)))
            previous-object))

(defcenum raster-operations
  (:srccopy     #x00CC0020)
  (:srcpaint    #x00EE0086)
  (:srcand      #x008800C6)
  (:srcinvert   #x00660046)
  (:srcerase    #x00440328)
  (:notsrccopy  #x00330008)
  (:notsrcerase #x001100A6)
  (:mergecopy   #x00C000CA)
  (:mergepaint  #x00BB0226)
  (:patcopy     #x00F00021)
  (:patpaint    #x00FB0A09)
  (:patinvert   #x005A0049)
  (:dstinvert   #x00550009)
  (:blackness   #x00000042)
  (:whiteness   #x00FF0062))

(libdefun bit-blit (source-hdc source-x source-y width height
                               target-hdc target-x target-y)
          (let ((success (foreign-funcall "BitBlt"
                                          :pointer target-hdc
                                          :int target-x
                                          :int target-y
                                          :int width
                                          :int height
                                          :pointer source-hdc
                                          :int source-x
                                          :int source-y
                                          :uint32 #.(foreign-enum-value 'raster-operations
                                                                        :blackness)
                                          :bool)))
            (unless success
              (error "Failed to blit: WinAPI error ~A" (last-system-error)))
            success))

;; (libdefun create-font (font-height is-bold
;;   (foreign-funcall "CreateFontA"
;;                    :int cHeight
;;                    :int cWidth
;;                    :int cEscapement
;;                    :int cOrientation
;;                    :int cWeight
;;                    :uint32 bItalic
;;                    :uint32 bUnderline
;;                    :uint32 bStrikeOut
;;                    :uint32 iCharSet
;;                    :uint32 iOutPrecision
;;                    :uint32 iClipPrecision
;;                    :uint32 iQuality
;;                    :uint32 iPitchAndFamily
;;                    :string pszFaceName
;;                    :pointer))
