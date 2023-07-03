(in-package #:org.shirakumo.alloy.uax-9)

(declaim (inline make-status-stack))
(defun make-status-stack ()
  (let ((stack (make-array 128 :element-type '(unsigned-byte 32))))
    (setf (aref stack 0) 0)
    stack))

(declaim (inline push-status))
(defun push-status (level override isolate stack)
  (declare (type level level))
  (declare (type (unsigned-byte 8) override))
  (declare (type boolean isolate))
  (declare (type stack stack))
  (setf (aref stack (incf (aref stack 0)))
        (logior (ash level 0)
                (ash override 8)
                (ash (if isolate 1 0) 16))))

(declaim (inline pop-status))
(defun pop-status (stack)
  (decf (aref stack 0)))

(declaim (inline last-level))
(defun last-level (stack)
  (ldb (byte 8 0) (aref stack (aref stack 0))))

(declaim (inline last-override))
(defun last-override (stack)
  (ldb (byte 8 8) (aref stack (aref stack 0))))

(declaim (inline last-isolate))
(defun last-isolate (stack)
  (= 1 (ldb (byte 8 16) (aref stack (aref stack 0)))))

(declaim (inline stack-empty-p))
(defun stack-empty-p (stack)
  (= 0 (aref stack 0)))

(declaim (inline empty-stack))
(defun empty-stack (stack)
  (setf (aref stack 0) 0)
  stack)

(declaim (inline stack-depth))
(defun stack-depth (stack)
  (aref stack 0))
