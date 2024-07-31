(uiop:define-package #:ck-os
  (:use #:cl #:ck-clle)
  (:use-reexport #+win32 #:ck-os/windows
                 #+unix #:ck-os/unix))

(in-package #:ck-os)

#+(or unix win32)
(defun check-program (program)
  "Check if a PROGRAM is available on the system path signal error if not."
  (check-type program string)
  (when (program-exists-p program)
    (error (format nil "Program '~a' unavailable in the system path."
                   program))))

(defun system-architecture ()
  "Determine the system architecture based on current Lisp features and return it as a string."
  (cond
    ((member :x86-64 *features*)
     "x86_64")
    ((member :x86 *features))
    (t "unknown")))

(defun os-string (&key (downcase t))
  "Determine the environment's operating system based on current Lisp features and return it as a
string.

If the DOWNCASE keyword argument is T, the resulting string is downcased."
  (let ((os-tag
          (cond
            ((member :win32 *features*) "Windows")
            ((member :linux *features*) "Linux")
            (t "Unknown"))))
    (if downcase (string-downcase os-tag) os-tag)))

(defun absolute-pathname (pathname)
  "Return the absolute pathname of the given PATHNAME.
If PATHNAME is already absolute, it is returned directly."
  (check-type pathname (or string pathname))
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname (uiop:getcwd))))

(defun relative-pathname (pathname)
  "Return the relative pathname from the current directory to PATHNAME.
If PATHNAME is relative, it is returned directly."
  (check-type pathname (or string pathname))
  (if (uiop:relative-pathname-p pathname)
      pathname
      (let* ((cwd (uiop:getcwd))
             (absolute (uiop:absolute-pathname-p pathname)))
        (if absolute
            (uiop:parse-native-namestring
             (subseq (uiop:native-namestring pathname) 
                     (length (uiop:native-namestring cwd))))
            (error "PATHNAME must be absolute or relative.")))))

(defun pathname-stem (pathname)
  "Extract and return the stem of the provided PATHNAME.
The stem is defined as the part of the PATH before the last slash."
  (check-type pathname (or string pathname))
  (let* ((pathname-name (namestring pathname))
         (sep (uiop:directory-separator-for-host pathname))
         (index (or (viewlet-position sep pathname-name :from-end t) 0))
         (stem (subseq pathname-name 0 index)))
    ;; If PATHNAME was an actual pathname, coerce the stem to be a pathname as
    ;; well, and not just a string.
    (if (typep pathname 'pathname)
        (pathname stem)
        stem)))

(defun copy-directory-contents (source-dir target-dir &key
                                            (clear nil)
                                            (on-conflict :error))
  "Copy contents of SOURCE-DIR to TARGET-DIR.

ON-CONFLICT controls the behavior when encountering files in TARGET-DIR
that already exist: :SUPERCEDE to overwrite, :IGNORE or NIL to skip, or :ERROR
to signal an error.

CLEAR controls whether or not the target directory is cleared before the files
from the source directory are transferred there."
  (check-type source-dir (or string pathname))
  (check-type target-dir (or string pathname))
  (when (pathnamep source-dir)
    (setf source-dir (namestring source-dir)))
  (when (pathnamep target-dir)
    (setf target-dir (namestring target-dir)))
  (uiop:ensure-directory-pathname source-dir)
  (ensure-directories-exist target-dir)
  (when clear
    (dolist (file (uiop:directory-files target-dir))
      (delete-file file)))
  (dolist (file (directory (concatenate 'string source-dir "*.*")))
    (let ((target-file (concatenate 'string
                                    target-dir
                                    (pathname-name file)
                                    "."
                                    (pathname-type file))))
      (cond
        ((and (eq on-conflict :supersede) (probe-file target-file))
         (uiop:copy-file file target-file))
        ((and (member on-conflict '(:ignore nil)) (probe-file target-file))
         nil)
        ((probe-file target-file)
         (error (format nil "File '~A' already exists in the target directory.
Adjust the ON-CONFFLICT key to supersede or ignore on conflict."
                        target-file)))
        (t
         (uiop:copy-file file target-file))))))
