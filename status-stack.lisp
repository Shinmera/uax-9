#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-9)

(defstruct (status-stack (:constructor make-status-stack ()))
  (stack (make-array (+ 1 MAX-DEPTH) :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (126)))
  (last -1 :type (signed-byte 8)))

(declaim (inline push-status))
(defun push-status (level override isolate stack)
  (declare (type level level))
  (declare (type (unsigned-byte 8) override))
  (declare (type boolean isolate))
  (setf (aref (status-stack-stack stack)
              (incf (status-stack-last stack)))
        (logior (ash level 0)
                (ash override 8)
                (ash (if isolate 1 0) 16))))

(declaim (inline pop-status))
(defun pop-status (stack)
  (decf (status-stack-last stack)))

(declaim (inline last-level))
(defun last-level (stack)
  (ldb (byte 8 0) (aref (status-stack-stack stack)
                        (status-stack-last stack))))

(declaim (inline last-override))
(defun last-override (stack)
  (ldb (byte 8 8) (aref (status-stack-stack stack)
                        (status-stack-last stack))))

(declaim (inline last-isolate))
(defun last-isolate (stack)
  (= 1 (ldb (byte 8 16) (aref (status-stack-stack stack)
                              (status-stack-last stack)))))

(declaim (inline stack-empty-p))
(defun stack-empty-p (stack)
  (= -1 (status-stack-last stack)))

(declaim (inline empty-stack))
(defun empty-stack (stack)
  (setf (status-stack-last stack) -1)
  stack)

(declaim (inline stack-depth))
(defun stack-depth (stack)
  (1+ (status-stack-last stack)))
