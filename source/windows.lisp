(defpackage #:io.windows
  (:use #:cl))

(in-package #:io.windows)

(defmethod window-shown-p (pid)
  "Check if a Windows window associated with a given PID is shown (visible)."
  (cffi:foreign-funcall "IsWindowVisible"
                        :int pid
                        :boolean))
