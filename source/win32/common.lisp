(defpackage #:ck-os/win32/common
  (:use #:cl #:ck-clle #:cffi)
  (:export #:+buffer-max-size+
           #:+string-encoding+
           #:make-rect
           #:rect-left
           #:rect-top
           #:rect-right
           #:rect-bottom))

(in-package #:ck-os/win32/common)
        
(defparameter +buffer-max-size+ 1024)

(defconstant +string-encoding+
  #+little-endian :ucs-2le
  #+big-endian :ucs-2be)

(defcstruct %rect
  (left   :long)
  (top    :long)
  (right  :long)
  (bottom :long))

(defclass rect ()
  ((pointer :initform #.(null-pointer) :reader pointer-of))
  (:documentation "A wrapper class for the internal Windows rectangle structure."))

(defun make-rect (&optional &key left top right bottom)
  (let ((rect (make-instance 'rect)))
    (let ((pointer (foreign-alloc '(:struct %rect)))
          (rect-left (or left 0))
          (rect-right (or right 0))
          (rect-top (or top 0))
          (rect-bottom (or bottom 0)))
      (handler-case
          (progn
            (check-type rect-left integer)
            (check-type rect-right integer)
            (check-type rect-top integer)
            (check-type rect-bottom integer)
            (setf (slot-value rect 'pointer) pointer)
            (with-foreign-slots ((left top right bottom) pointer (:struct %rect))
              (setf left rect-left
                    top rect-top
                    right rect-right
                    bottom rect-bottom))
            (tg:finalize rect (lambda () (foreign-free pointer))))
        (error (e)
          (foreign-free pointer)
          (error e))))))

(defmethod rect-left ((rect rect))
  (foreign-slot-value (pointer-of rect) '(:struct %rect) 'left))

(defmethod (setf rect-left) ((value integer) (rect rect))
  (setf (foreign-slot-value (pointer-of rect) '(:struct %rect) 'left) value))

(defmethod rect-top ((rect rect))
  (foreign-slot-value (pointer-of rect) '(:struct %rect) 'top))

(defmethod (setf rect-top) ((value integer) (rect rect))
  (setf (foreign-slot-value (pointer-of rect) '(:struct %rect) 'top) value))

(defmethod rect-right ((rect rect))
  (foreign-slot-value (pointer-of rect) '(:struct %rect) 'right))

(defmethod (setf rect-right) ((value integer) (rect rect))
  (setf (foreign-slot-value (pointer-of rect) '(:struct %rect) 'right) value))

(defmethod rect-bottom ((rect rect))
  (foreign-slot-value (pointer-of rect) '(:struct %rect) 'bottom))

(defmethod (setf rect-bottom) ((value integer) (rect rect))
  (setf (foreign-slot-value (pointer-of rect) '(:struct %rect) 'bottom) value))
