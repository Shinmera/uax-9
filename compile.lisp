#|
 This file is a part of UAX-14
 (c) 20114 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-9.compiler
  (:use #:cl)
  (:local-nicknames
   (#:uax-9 #:org.shirakumo.alloy.uax-9))
  (:export
   #:compile-databases))

(in-package #:org.shirakumo.alloy.uax-9.compiler)

(defun set-line-class (data line)
  (let* ((end (or (position #\# line) (length line)))
         (sep (position #\; line))
         (dot (position #\. line))
         (class (string-upcase (string-trim " " (subseq line (1+ sep) end))))
         (value (uax-9::class-id (find-symbol class "KEYWORD")))
         (start (parse-integer line :radix 16 :start 0 :end (or dot sep)))
         (end (if dot (parse-integer line :radix 16 :start (+ 2 dot) :end sep) start)))
    (loop for i from start to end
          do (setf (aref data i) value))))

(defun compile-bidi-class-database (&key (source (make-pathname :name "DerivedBidiClass" :type "txt" :defaults uax-9::*here*))
                                         (target uax-9:*bidi-class-database-file*))
  (let ((data uax-9::+bidi-class-map+))
    (with-open-file (stream source
                            :direction :input
                            :element-type 'character
                            :external-format :utf-8)
      (loop for line = (read-line stream NIL)
            while line
            do (when (and (string/= "" line)
                          (char/= #\# (char line 0)))
                 (set-line-class data line))))
    (with-open-file (stream target
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence data stream))
    target))

(defun write-u32le (int stream)
  (write-byte (ldb (byte 8 0) int) stream)
  (write-byte (ldb (byte 8 8) int) stream)
  (write-byte (ldb (byte 8 16) int) stream)
  (write-byte (ldb (byte 8 24) int) stream))

(defun set-bracket-pair (data line)
  (let* ((from (position #\; line))
         (to (position #\; line :start (1+ from))))
    (let ((from (parse-integer line :end from :radix 16))
          (to (parse-integer line :start (+ from 2) :end to :radix 16))
          (type (ecase (char line (+ to 2))
                  (#\o 0)
                  (#\c 1))))
      (setf (gethash from data) (logior to (ash type 25))))))

(defun compile-bidi-brackets-table (&key (source (make-pathname :name "BidiBrackets" :type "txt" :defaults uax-9::*here*))
                                         (target uax-9:*bidi-brackets-table-file*))
  (let ((data uax-9::+bidi-brackets-map+))
    (with-open-file (stream source
                            :direction :input
                            :element-type 'character
                            :external-format :utf-8)
      (loop for line = (read-line stream NIL)
            while line
            do (when (and (string/= "" line)
                          (char/= #\# (char line 0)))
                 (set-bracket-pair data line))))
    (with-open-file (stream target
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-u32le (hash-table-count data) stream)
      (loop for key being the hash-keys of data
            for val being the hash-values of data
            do (write-u32le key stream)
               (write-u32le val stream)))
    target))

(defun set-mirror-pair (data line)
  (let ((from (position #\; line))
        (to (position #\# line)))
    (let ((from (parse-integer line :end from :radix 16))
          (to (parse-integer line :start (+ 2 from) :end to :radix 16)))
      (setf (gethash from data) to))))

(defun compile-bidi-mirror-table (&key (source (make-pathname :name "BidiMirroring" :type "txt" :defaults uax-9::*here*))
                                         (target uax-9:*bidi-mirroring-table-file*))
  (let ((data uax-9::+bidi-mirroring-map+))
    (with-open-file (stream source
                            :direction :input
                            :element-type 'character
                            :external-format :utf-8)
      (loop for line = (read-line stream NIL)
            while line
            do (when (and (string/= "" line)
                          (char/= #\# (char line 0)))
                 (set-mirror-pair data line))))
    (with-open-file (stream target
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-u32le (hash-table-count data) stream)
      (loop for key being the hash-keys of data
            for val being the hash-values of data
            do (write-u32le key stream)
               (write-u32le val stream)))
    target))

(defun compile-databases ()
  (compile-bidi-class-database)
  (compile-bidi-brackets-table)
  (compile-bidi-mirror-table))
