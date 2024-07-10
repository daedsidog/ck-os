(defpackage #:ck.io.environment
  (:use #:cl #:ck.clle))

(in-package #:ck.io.environment)

(defun* check-program (program)
  "Check if a PROGRAM is available on the system's PATH, signal error if not."
  (check-type program string)
  (let ((test-command (if (member :win32 *features*)
                          (format nil "where ~a" program)
                          (format nil "command -v ~a" program))))
    (let ((exit-code (nth-value 2 (uiop:run-program test-command
                                                    :ignore-error-status t))))
      (unless (= exit-code 0)
        (error (format nil "Program '~a' is not available in the system path."
                       program))))))

(defun* host-arch ()
  "Determine the host architecture based on current Lisp features and return it
as a string."
  (cond
    ((member :x86-64 *features*)
     "x86_64")
    (t "x86")))

(defun* host-os (&key (downcase t))
  "Determine the host operating system based on current Lisp features and return
it as a string.
If the DOWNCASE keyword argument is T, the resulting string is downcased."
  (let ((os-tag
          (cond
            ((member :win32 *features*) "Windows")
            ((member :linux *features*) "Linux")
            (t "Unknown"))))
    (if downcase (string-downcase os-tag) os-tag)))

(defun* absolute-pathname (pathname)
  "Return the absolute pathname of the given PATHNAME.
If PATHNAME is already absolute, it is returned directly."
  (check-type pathname (or string pathname))
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname (uiop:getcwd))))

(defun* relative-pathname (pathname)
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

(defun* pathname-stem (pathname)
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

(defun* copy-directory-contents (source-dir target-dir &key
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

(defun* host-cache-directory ()
  "Return the cache directory for the current host as a pathname.

Defaults to Unix-like systems' cache directory.
Returns an error if the host OS is not recognized or its cache directory could not be determined."
  (cond ((uiop:os-windows-p)
         (let ((username (uiop:getenv "USERNAME")))
           (uiop:parse-native-namestring
            (uiop:native-namestring
             (format nil "C:\\Users\\~A\\AppData\\Local\\" username)))))
        ((uiop:os-macosx-p)
         (uiop:parse-native-namestring
          (uiop:native-namestring "~/Library/Caches/")))
        ((uiop:os-unix-p)
         (uiop:parse-native-namestring
          (uiop:native-namestring "~/.cache/")))
        (t (error "Could not determine the cache directory for this host."))))
