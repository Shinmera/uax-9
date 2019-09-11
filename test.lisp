#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-9.test
  (:use #:cl #:parachute)
  (:shadow #:test)
  (:local-nicknames
   (#:uax-9 #:org.shirakumo.alloy.uax-9))
  (:export #:test #:uax-9))

(in-package #:org.shirakumo.alloy.uax-9.test)

(defun test (&optional (type 'quiet))
  (let* ((report (parachute:test 'uax-9 :report type))
         (total (length (results report)))
         (passed (length (results-with-status :passed report)))
         (failed (length (results-with-status :failed report))))
    (format *terminal-io* "~&
Total:  ~9,,'':d
Passed: ~9,,'':d (~2d%)
Failed: ~9,,'':d (~2d%)~%"
            total passed (round (/ passed total 1/100))
            failed (round (/ failed total 1/100)))))

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

(defun maybe-parse (x)
  (if (string-equal x "x")
      T
      (parse-integer x)))

(defun pop-explicit-or-bn (reorder string)
  (coerce (loop for idx across reorder
                for class = (uax-9::class-at string idx)
                unless (or (= class (uax-9::class-id :BN))
                           (<= (uax-9::class-id :LRE) class (uax-9::class-id :PDF)))
                collect idx)
          'vector))

(defun test-entry (i string dir levels reorder &optional level)
  (multiple-value-bind (result-levels found-level)
      (eval-in-context
       *context*
       (make-instance 'comparison-result
                      :expression `(is level= ,levels (uax-9:levels ,string :paragraph-direction ,dir))
                      :value-form `(uax-9:levels ,string :paragraph-direction ,dir)
                      :expected levels
                      :body (lambda () (uax-9:levels string :paragraph-direction dir))
                      :comparison 'level=
                      :description (format NIL "#~d" i)))
    (when level
      (is = level found-level))
    (eval-in-context
     *context*
     (make-instance 'comparison-result
                    :expression `(is equal ,reorder (uax-9:reorder ,result-levels))
                    :value-form `(uax-9:reorder ,result-levels)
                    :expected reorder
                    :body (lambda () (pop-explicit-or-bn (uax-9:reorder result-levels)
                                                         string))
                    :comparison 'level=
                    :description (format NIL "#~d" i)))))

(define-test bidi-test
  :parent uax-9
  (with-open-file (stream (make-pathname :name "BidiTest" :type "txt" :defaults uax-9::*here*)
                          :direction :input
                          :element-type 'character
                          :external-format :utf-8)
    (loop with levels and reorder
          for i from 1
          for line = (read-line stream NIL)
          while line
          do (when (and (string/= "" line) (char/= #\# (char line 0)))
               (cond ((prefix-p "@Levels:" line)
                      (setf levels (map 'vector #'maybe-parse (split #\  line :start (length "@Levels: ")))))
                     ((prefix-p "@Reorder:" line)
                      (setf reorder (map 'vector #'parse-integer (split #\  line :start (length "@Reorder: ")))))
                     ((prefix-p "@" line))
                     (T
                      (destructuring-bind (input bitset) (split #\; line)
                        (let* ((bitset (parse-integer (string-trim " " bitset)))
                               (types (loop for part in (split #\  input)
                                            collect (find-symbol part "KEYWORD")))
                               (string (string-for-bidi-classes types)))
                          (flet ((test (dir)
                                   (test-entry i string dir levels reorder)))
                            (when (logtest 1 bitset) (test :auto))
                            (when (logtest 2 bitset) (test :left-to-right))
                            (when (logtest 4 bitset) (test :right-to-left))))))))
             (when (= 0 (mod i 100000))
               (format T "~& ~8,,'':d lines processed." i)))))

(define-test bidi-character-test
  :parent uax-9
  (with-open-file (stream (make-pathname :name "BidiCharacterTest" :type "txt" :defaults uax-9::*here*)
                          :direction :input
                          :element-type 'character
                          :external-format :utf-8)
    (loop for i from 1
          for line = (read-line stream NIL)
          while line
          do (when (and (string/= "" line) (char/= #\# (char line 0)))
               (destructuring-bind (points dir level levels reorder) (split #\; line)
                 (let* ((points (map 'vector (lambda (x) (parse-integer x :radix 16)) (split #\Space points)))
                        (string (map 'string #'code-char points))
                        (dir (cond ((string= dir "0") :left-to-right)
                                   ((string= dir "1") :right-to-left)
                                   ((string= dir "2") :auto)))
                        (level (parse-integer level))
                        (levels (map 'vector #'maybe-parse (split #\Space levels)))
                        (reorder (map 'vector #'parse-integer (split #\Space reorder))))
                   (test-entry i string dir levels reorder level))))
             (when (= 0 (mod i 10000))
               (format T "~& ~8,,'':d lines processed." i)))))
