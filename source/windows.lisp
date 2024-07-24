(defpackage #:ck-os/windows
  (:use #:cl)
  (:export #:window-shown-p
           #:program-exists-p
           #:cache-directory
           #:temporary-directory))

(in-package #:ck-os/windows)

(defun window-shown-p (pid)
  "Check if a Windows window associated with a given PID is shown (visible)."
  (cffi:foreign-funcall "IsWindowVisible"
                        :int pid
                        :boolean))

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
