(defpackage #:ck-os/windows
  (:use #:cl #:ck-clle #:cffi)
  (:export
   ;; GENERAL
   #:program-exists-p
   #:cache-directory
   #:temporary-directory
   ;; USER32
   #:open-process-handle
   #:close-handle
   #:read-process-memory
   #:with-process-handle
   #:window-visible-p
   #:process-windows
   #:process-visible-windows
   #:window-device-context
   #:window-dimensions
   #:window-resolution
   ;; GDI32
   #:create-solid-brush
   #:destroy-gdi-object
   #:fill-rectangle))

(in-package #:ck-os/windows)

;;; GENERAL

(defparameter +default-buffer-max-size+ 1024)

(defun program-exists-p (program)
  "Returns non-nil if PROGRAM is available on the system path, nil otherwise for Win32 systems."
  (multiple-value-bind (1st 2nd exit-code) (uiop:run-program (format nil "where \"~A\"" program)
                                                             :ignore-error-status t)
    (declare (ignore 1st 2nd))
    (if (/= exit-code 0)
        nil
        t)))

(defun cache-directory ()
  "Return the cache directory for the current system as a pathname."
         (let ((username (uiop:getenv "USERNAME")))
           (uiop:parse-native-namestring
            (uiop:native-namestring
             (format nil "C:\\Users\\~A\\AppData\\Local\\" username)))))

(defun temporary-directory ()
  "Return the temporary directory for the current system as a pathname."
  (let ((username (uiop:getenv "USERNAME")))
    (uiop:parse-native-namestring
     (uiop:native-namestring
      (format nil "C:\\Users\\~A\\AppData\\Local\\Temp\\" username)))))

;; USER32

(defvar *user32-library-loaded* nil)

(defmacro defun-user32 (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (unless *user32-library-loaded*
       (load-foreign-library "user32.dll")
       (setf *user32-library-loaded* t))
     ,@body))

(defun-user32 last-system-error ()
  (foreign-funcall "GetLastError" :uint32))

(defcenum process-access-rights
  (:all-access       #x001f0fff)
  (:read             #x80000000)
  (:write            #x40000000)
  (:duplicate-handle #x00000040))

(defun-user32 open-process-handle (pid &optional process-access-rights inherit-handle)
  (let ((process-access-rights (or process-access-rights
                                   (foreign-enum-value 'process-access-rights
                                                            :all-access)))
        (inherit-handle (or inherit-handle nil)))
    (let ((handle (foreign-funcall "OpenProcess"
                                        :uint32 process-access-rights
                                        :bool inherit-handle
                                        :uint32 pid
                                        :pointer)))
      (when (eq handle #.(null-pointer))
        (error "Could not open process ~A: WinAPI error ~A"
               handle
               (last-system-error)))
      handle)))

(defun-user32 close-handle (handle)
  (let ((success (foreign-funcall "CloseHandle"
                                       :pointer handle
                                       :bool)))
    (unless success
      (error "Could not close handle ~A: WinAPI error ~A"
             handle
             (last-system-error)))
    success))

(defmacro with-process-handle ((handle pid) &body body)
  `(let ((,handle (open-process-handle ,pid)))
     (unwind-protect
          (progn ,@body)
       (close-handle ,handle))))

(defgeneric read-process-memory (process address type &optional max-size)
  (:documentation "Read memory at ADDRESS and return native type.

If PROCESS is a handle, it will not be closed by this function.
PROCESS can also be a PID of the process.

This function returns the native Lisp type associated with the foreign type."))

(defun-user32 %read-process-memory (process-handle address type &optional max-size)
  (let* ((type (if (member type '(:dword))
                   :uint32
                   type))
         (size (if (eql type :string)
                   (or max-size +default-buffer-max-size+)
                   (foreign-type-size type))))
    (with-foreign-pointer (buffer size)
      (let ((success (foreign-funcall "ReadProcessMemory"
                                           :pointer process-handle
                                           :pointer (make-pointer address)
                                           :pointer buffer
                                           :uint32 size
                                           :pointer #.(null-pointer)
                                           :bool)))
        (unless success
          (error "Could not read process ~A memory at ~A: WinAPI error ~A"
                 process-handle address (last-system-error)))
        (if (eql type :string)
            (loop for i from 0 below size
                  for char = (mem-aref buffer :char i)
                  while (/= char 0)
                  collect (code-char char) into chars
                  finally (return (concatenate 'string chars)))
            (mem-ref buffer type))))))

(defmethod read-process-memory ((pid integer) address type &optional max-size)
  (let ((process-handle (open-process-handle pid)))
    (unwind-protect
         (%read-process-memory process-handle address type max-size)
      (close-handle process-handle))))

;; There might be a problem with the read-time evaluation here on some platforms where the pointer
;; type resolves to a fixnum, as it could introduce a specialization onflict with the above method.
(defmethod read-process-memory ((process-handle #.(class-of (null-pointer)))
                                address type &optional max-size)
  (%read-process-memory process-handle address type max-size))

(let ((pid-window-table (make-hash-table)))
  
  (defcallback window-enumeration-callback :bool ((window-handle :pointer) (pid-store :pointer))
    (with-foreign-object (pid :uint32)
      (let ((tid (foreign-funcall "GetWindowThreadProcessId"
                               :pointer window-handle
                               :pointer pid
                               :uint32)))
        (when (zerop tid)
          (error "Could not get creator PID of window ~A: WinAPI error ~A"
                 window-handle
                 (last-system-error))))
      (when (= (mem-ref pid-store :uint32) (mem-ref pid :uint32))
        (push window-handle (gethash (mem-ref pid :uint32) pid-window-table)))
      t))

  (defun-user32 process-windows (pid)
    "Return a list of all the windows associated with the PID."
        (with-foreign-object (pid-store :uint32)
      (setf (mem-ref pid-store :uint32) pid)
      (setf (gethash pid pid-window-table) ())
      (unless (foreign-funcall "EnumWindows"
                               :pointer (callback window-enumeration-callback)
                               :pointer pid-store
                               :bool)
        (error "Could not enumerate windows of PID ~A: WinAPI error ~A"
               pid
               (last-system-error)))
      (prog1 (gethash pid pid-window-table)
        (remhash pid pid-window-table)))))

(defun-user32 window-visible-p (window-handle)
  "Check if a Windows window associated with a given WINDOW-HANDLE is shown (visible)."
    (foreign-funcall "IsWindowVisible"
                   :pointer window-handle
                   :boolean))

(defun-user32 process-visible-windows (pid)
  "Return a list of the visible window handles of the PID."
  (remove-if-not #'window-visible-p (process-windows pid)))

(defcstruct rect
  (left   :long)
  (top    :long)
  (right  :long)
  (bottom :long))

(defun-user32 window-dimensions (window-handle)
    (with-foreign-object (rect '(:struct rect))
    (unless (foreign-funcall "GetClientRect"
                                  :pointer window-handle
                                  :pointer rect
                                  :bool)
      (error "Could not get window dimensions of ~A: WinAPI error ~A"
             window-handle
             (last-system-error)))
    (values (foreign-slot-value rect '(:struct rect) 'left)
            (foreign-slot-value rect '(:struct rect) 'top)
            (foreign-slot-value rect '(:struct rect) 'right)
            (foreign-slot-value rect '(:struct rect) 'bottom))))

(defun-user32 window-resolution (window-handle)
    (multiple-value-bind (left top right bottom) (window-dimensions window-handle)
    (declare (ignore left top))
    (values right bottom)))

(defun-user32 release-device-context (window-handle hdc)
  "Release the device context of the WINDOW-HANDLE."
    (foreign-funcall "ReleaseDC"
                   :pointer window-handle
                   :pointer hdc
                   :int))

;;; GDI32

(defvar *gdi32-library-loaded* nil)

(defmacro defun-gdi32 (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (unless *gdi32-library-loaded*
       (load-foreign-library "gdi32.dll")
       (setf *gdi32-library-loaded* t))
     ,@body))
  
(defun-gdi32 window-device-context (window-handle)
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

(defun-gdi32 device-capabilities (device-context index)
  (foreign-funcall "GetDeviceCaps"
                   :pointer device-context
                   :int (foreign-enum-value 'device-capabilities-index
                                            index)
                   :int))

(defun-gdi32 create-solid-brush (color)
  (let ((brush (foreign-funcall "CreateSolidBrush"
                                :uint32 color
                                :pointer)))
    (when (eq brush #.(null-pointer))
      (error "Failed to create solid brush: WinAPI error ~A" (last-system-error)))
    brush))

(defun-gdi32 destroy-gdi-object (object)
  (let ((success (foreign-funcall "DeleteObject"
                                  :pointer object
                                  :bool)))
    (unless success
      (error "Failed to delete GDI object: WinAPI error ~A" (last-system-error)))
    success))

(defun-gdi32 fill-rectangle (device-context brush &key left right top bottom)
  (with-foreign-object (rect '(:struct rect))
    (setf (foreign-slot-value rect '(:struct rect) 'left) left)
    (setf (foreign-slot-value rect '(:struct rect) 'right) right)
    (setf (foreign-slot-value rect '(:struct rect) 'top) top)
    (setf (foreign-slot-value rect '(:struct rect) 'bottom) bottom)
    (let ((success (foreign-funcall "FillRect"
                                    :pointer device-context
                                    :pointer rect
                                    :pointer brush
                                    :bool)))
      (unless success
        (error "Failed to fill rectangle: WinAPI error ~A" (last-system-error)))
      success)))
