#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-9.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:uax-9 #:org.shirakumo.alloy.uax-9))
  (:export #:uax-9))

(in-package #:org.shirakumo.alloy.uax-9.test)

(define-test uax-9)

(defun split (split string &key (start 0) (end (length string)))
  (let ((parts ()) (buffer (make-string-output-stream)))
    (flet ((maybe-output ()
             (let ((part (get-output-stream-string buffer)))
               (when (string/= part "") (push part parts)))))
      (loop for i from start below end
            for char = (char string i)
            do (if (char= char split)
                   (maybe-output)
                   (write-char char buffer))
            finally (maybe-output))
      (nreverse parts))))

(defun prefix-p (prefix line)
  (and (<= (length prefix) (length line))
       (string-equal prefix line :end2 (length prefix))))

(defun level= (as bs)
  (loop for a across as
        for b across bs
        always (or (eql T a)
                   (eql T b)
                   (= a b))))

(defvar +class-character-map+
  (loop for class across uax-9::+bidi-class-list+
        collect (cons class (loop for i from 0 to #xFFFF
                                  do (when (= (uax-9::class-id class) (uax-9::bidi-class i))
                                       (return (code-char i)))))))

(defun string-for-bidi-classes (classes)
  (map 'string (lambda (c) (cdr (assoc c +class-character-map+))) classes))

(define-test bidi-test
  :parent uax-9
  (with-open-file (stream (make-pathname :name "BidiTest" :type "txt" :defaults uax-9::*here*)
                          :direction :input
                          :element-type 'character
                          :external-format :utf-8)
    (loop with levels and reorder
          for line = (read-line stream NIL)
          while line
          do (when (and (string/= "" line) (char/= #\# (char line 0)))
               (cond ((prefix-p "@Levels:" line)
                      (setf levels (loop for part in (split #\  line :start (length "@Levels:"))
                                         collect (if (string-equal part "x")
                                                     T
                                                     (parse-integer part)))))
                     ((prefix-p "@Reorder:" line)
                      (setf reorder (loop for part in (split #\  line :start (length "@Reorder:"))
                                          collect (parse-integer part))))
                     ((prefix-p "@" line))
                     (T
                      (destructuring-bind (input bitset) (split #\; line)
                        (let* ((bitset (parse-integer (string-trim " " bitset)))
                               (types (loop for part in (split #\  input)
                                            collect (find-symbol part "KEYWORD")))
                               (string (string-for-bidi-classes types)))
                          (flet ((test (level)
                                   (let* ((result-levels (finish (uax-9::run-algorithm string level)))
                                          (%levels (is level= levels (uax-9::levels string level result-levels))))
                                     (is equal reorder (uax-9::reorder %levels)))))
                            (when (logtest 1 bitset)
                              (test 0))
                            (when (logtest 2 bitset)
                              (test 2))
                            (when (logtest 4 bitset)
                              (test 3)))))))))))

(define-test bidi-character-test
  :parent uax-9)
