(defpackage #:ck-os/win32/common
  (:use #:cl #:ck-clle #:cffi)
  (:export #:+buffer-max-size+
           #:+string-encoding+
           #:rect
           #:make-rect
           #:rect-left
           #:rect-top
           #:rect-right
           #:rect-bottom
           #:rect-width
           #:rect-height
           #:rect-x-incf
           #:rect-y-incf
           #:rect-x-decf
           #:rect-y-decf
           #:rect-incf
           #:rect-inc
           #:rect-decf
           #:rect-dec
           #:normalized-rect))

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

(defmethod copy-rect ((r rect))
  (make-rect :left   (rect-left   r)
             :right  (rect-right  r)
             :top    (rect-top    r)
             :bottom (rect-bottom r)))

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

(defmethod rect-width ((rect rect))
  (- (rect-right rect) (rect-left rect)))

(defmethod rect-height ((rect rect))
  (- (rect-bottom rect) (rect-top rect)))

(defmethod rect-x-incf ((rect rect) &optional (delta 1))
  (incf (rect-left rect) delta)
  (incf (rect-right rect) delta))

(defmethod rect-y-incf ((rect rect) &optional (delta 1))
  (incf (rect-top rect) delta)
  (incf (rect-bottom rect) delta))

(defmethod rect-x-decf ((rect rect) &optional (delta 1))
  (incf (rect-left rect) (- delta))
  (incf (rect-right rect) (- delta)))

(defmethod rect-y-decf ((rect rect) &optional (delta 1))
  (incf (rect-top rect) (- delta))
  (incf (rect-bottom rect) (- delta)))

(defmethod rect-incf ((rect rect) &optional (delta 1))
  (decf (rect-left rect) delta)
  (decf (rect-top rect) delta)
  (incf (rect-right rect) delta)
  (incf (rect-bottom rect) delta))

(defmethod rect-decf ((rect rect) &optional (delta 1))
  (incf (rect-left rect) delta)
  (incf (rect-top rect) delta)
  (decf (rect-right rect) delta)
  (decf (rect-bottom rect) delta))

(defmethod rect-inc ((rect rect) &optional (delta 1))
  (let ((new-rect (make-rect
                   :left (rect-left rect)
                   :top (rect-top rect)
                   :right (rect-right rect)
                   :bottom (rect-bottom rect))))
    (rect-incf new-rect delta)
    new-rect))

(defmethod rect-dec ((rect rect) &optional (delta 1))
  (let ((new-rect (make-rect
                   :left (rect-left rect)
                   :top (rect-top rect)
                   :right (rect-right rect)
                   :bottom (rect-bottom rect))))
    (rect-decf new-rect delta)
    new-rect))

(defmethod normalized-rect ((rect rect))
  (make-rect :right (rect-width rect)
             :bottom (rect-height rect)))
