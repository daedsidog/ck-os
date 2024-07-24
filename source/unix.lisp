(defpackage #:ck-os/unix
  (:use #:cl)
  (:export #:program-exists-p
           #:cache-directory
           #:temporary-directory))

(defun program-exists-p (program)
  "Returns non-nil if PROGRAM is available on the system path, nil otherwise for Unix systems."
  (multiple-value-bind (1st 2nd exit-code) (uiop:run-program (format nil
                                                                     "command -v \"~A\"" program)
                                                             :ignore-error-status t)
    (declare (ignore 1st 2nd))
    (if (/= exit-code 0)
        nil
        t)))

(defun cache-directory ()
  "Return the cache directory for the current system as a pathname."
  (uiop:parse-native-namestring
   (uiop:native-namestring "~/.cache/")))

(defun temporary-directory ()
  "Return the temporary directory for the current system as a pathname."
  (uiop:parse-native-namestring
   (uiop:native-namestring "/tmp/")))
