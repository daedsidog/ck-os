(defpackage #:ck-os/win32/user32
  (:use #:cl #:ck-clle #:cffi #:ck-os/win32/common)
  (:export #:open-process-handle
           #:close-handle
           #:read-process-memory
           #:with-process-handle
           #:list-windows
           #:window-visible-p
           #:windows-containing-title-substring
           #:list-visible-windows
           #:windows-of-pid
           #:visible-windows-of-pid
           #:window-device-context
           #:window-dimensions
           #:+tff-top-align+
           #:+tff-left-align+
           #:+tff-center-align+
           #:+tff-right-align+
           #:+tff-vertical-center-align+
           #:+tff-bottom-align+
           #:+tff-word-break+
           #:+tff-single-line+
           #:+tff-expand-tabs+
           #:+tff-tab-stop+
           #:+tff-no-clip+
           #:+tff-external-leading+
           #:+tff-calculate-rectangle+
           #:+tff-no-prefix+
           #:draw-text))

(in-package #:ck-os/win32/user32)

(defvar *library-loaded* nil)

(defmacro libdefun (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (unless *library-loaded*
       (load-foreign-library "user32.dll")
       (setf *library-loaded* t))
     ,@body))

(libdefun last-system-error ()
  (foreign-funcall "GetLastError" :uint32))

(defcenum process-access-rights
  (:all-access       #x001f0fff)
  (:read             #x80000000)
  (:write            #x40000000)
  (:duplicate-handle #x00000040))

(libdefun open-process-handle (pid &optional process-access-rights inherit-handle)
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

(libdefun close-handle (handle)
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

(libdefun %read-process-memory (process-handle address type &optional max-size)
  (let* ((type (if (member type '(:dword))
                   :uint32
                   type))
         (size (if (eql type :string)
                   (or max-size +buffer-max-size+)
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

(let ((busy nil)
      (windows ()))
  
  (defcallback %window-enumeration-callback :bool ((window-handle :pointer) (unused :pointer))
    (declare (ignore unused))
    (push window-handle windows))

  (libdefun list-windows ()
    "Return a list of all top-level windows."
    (loop while busy)
    (setf busy t)
    (setf windows ())
    (unwind-protect 
         (progn
           (with-foreign-object (dummy-pointer :pointer)
             (unless (foreign-funcall "EnumWindows"
                                      :pointer (callback %window-enumeration-callback)
                                      ;; This pointer is unused, but required to prevent a memory
                                      ;; fault.  I don't know why this happens.
                                      :pointer dummy-pointer
                                      :bool)
               (error "Could not enumerate windows: WinAPI error ~A"
                      (last-system-error))))
           windows)
      (when busy
        (setf busy nil)
        (setf windows ())))))

(libdefun window-pid (window-handle)
  (with-foreign-object (pid :uint32)
    (let ((tid (foreign-funcall "GetWindowThreadProcessId"
                                :pointer window-handle
                                :pointer pid
                                :uint32)))
      (when (zerop tid)
        (error "Could not get creator PID of window ~A: WinAPI error ~A"
               window-handle
               (last-system-error)))
      (mem-ref pid :uint32))))

(libdefun windows-of-pid (pid)
  "Return a list of all the windows associated with the PID."
  (remove-if-not (lambda (wh) (eq (window-pid wh) pid)) (list-windows)))

(libdefun window-visible-p (window-handle)
  "Check if a Windows window associated with a given WINDOW-HANDLE is shown (visible)."
  (foreign-funcall "IsWindowVisible"
                   :pointer window-handle
                   :boolean))

(libdefun list-visible-windows ()
  "Return a list of all visible top-level windows."
  (remove-if-not #'window-visible-p (list-windows)))

(libdefun visible-windows-of-pid (pid)
  "Return a list of the visible window handles of the PID."
  (remove-if-not #'window-visible-p (windows-of-pid pid)))

(libdefun window-dimensions (window-handle)
  (let ((rect (make-rect)))
    (unless (foreign-funcall "GetClientRect"
                             :pointer window-handle
                             :pointer (pointer-of rect)
                             :bool)
      (error "Could not get window dimensions of ~A: WinAPI error ~A"
             window-handle
             (last-system-error)))
    rect))

(libdefun release-device-context (window-handle hdc)
  "Release the device context of the WINDOW-HANDLE."
  (foreign-funcall "ReleaseDC"
                   :pointer window-handle
                   :pointer hdc
                   :int))

(defun window-title (window-handle)
  "Retrieve the title of the window associated with WINDOW-HANDLE."
  (with-foreign-pointer (title +buffer-max-size+)
    (let ((string-length (foreign-funcall "GetWindowTextW"
                                          :pointer window-handle
                                          :pointer title
                                          :int +buffer-max-size+
                                          :uint32)))
      (unless (zerop string-length)
        (foreign-string-to-lisp title :encoding +string-encoding+)))))

(libdefun windows-containing-title-substring (substring)
  "Return a list of all the windows whose titles contain the SUBSTRING.  Case insensitive."
  (remove-if-not (lambda (wh) (search substring (window-title wh) :test #'equalp))
                 (list-windows)))

(defcenum text-format-flags
  (:top-align                 #x000)
  (:left-align                #x000)
  (:center-align              #x001)
  (:right-align               #x002)
  (:vertical-center-align     #x004)
  (:bottom-align              #x008)
  (:word-break                #x010)
  (:single-line               #x020)
  (:expand-tabs               #x040)
  (:tab-stop                  #x080)
  (:no-clip                   #x100)
  (:external-leading          #x200)
  (:calculate-rectangle       #x400)
  (:no-prefix                 #x800))

(defconstant +tff-top-align+               #x000)
(defconstant +tff-left-align+              #x000)
(defconstant +tff-center-align+            #x001)
(defconstant +tff-right-align+             #x002)
(defconstant +tff-vertical-center-align+   #x004)
(defconstant +tff-bottom-align+            #x008)
(defconstant +tff-word-break+              #x010)
(defconstant +tff-single-line+             #x020)
(defconstant +tff-expand-tabs+             #x040)
(defconstant +tff-tab-stop+                #x080)
(defconstant +tff-no-clip+                 #x100)
(defconstant +tff-external-leading+        #x200)
(defconstant +tff-calculate-rectangle+     #x400)
(defconstant +tff-no-prefix+               #x800)

;; ASCII text support only
(libdefun draw-text (hdc text length rect format)
  (with-foreign-string (string text)
    (let ((result (foreign-funcall "DrawTextA"
                                   :pointer hdc
                                   :pointer string
                                   :int length
                                   :pointer (pointer-of rect)
                                   :uint32 format
                                   :int)))
      (when (zerop result) 
        (error "Failed to draw text on ~A: WinAPI error ~A" hdc (last-system-error)))
      result)))
