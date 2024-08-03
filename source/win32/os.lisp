(uiop:define-package #:ck-os/win32
  (:use #:cl #:ck-clle #:cffi)
  (:use-reexport #:ck-os/win32/common
                 #:ck-os/win32/user32
                 #:ck-os/win32/gdi32))

(defpackage #:ck-os/win32/os
  (:use #:cl #:ck-clle)
  (:export #:program-exists-p
           #:cache-directory
           #:temporary-directory))

(in-package #:ck-os/win32/os)

(defun program-exists-p (program)
  "Returns non-nil if PROGRAM is available on the system path, nil otherwise for Win32 systems."
  (multiple-value-bind (1st 2nd exit-code) (uiop:run-program (format nil "where \"~A\"" program)
                                                             :ignore-error-status t)
    (declare (ignore 1st 2nd))
    (if (/= exit-code 0) nil t)))

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
