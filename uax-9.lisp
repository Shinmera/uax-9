#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-9)

(defun bidi-string-p (string)
  (declare (optimize speed))
  (declare (type string string))
  (not (loop for char across string
             for class = (bidi-class (char-code char))
             always (or (class= class :L) ; Early out
                        (and ; Ensure it's none of the RTL classes.
                         (not (class= class :R))
                         (not (class= class :AL))
                         (not (class= class :AN))
                         (not (class<= :LRE class :PDI)))))))

(defun make-class-array (string)
  (let ((array (make-array (length string) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string))
      (setf (aref array i) (class-at string i)))
    array))

(defun determine-matching-isolates (classes)
  (let ((matching-pdis (make-array (length classes) :element-type 'fixnum :initial-element -1))
        (matching-initiator (make-array (length classes) :element-type 'fixnum :initial-element -1)))
    (loop for i from 0 below (length classes)
          for class = (aref classes i)
          do (when (or (class= class :LRI)
                       (class= class :RLI)
                       (class= class :FSI))
               (loop with depth = 1
                     for j from (1+ i) below (length classes)
                     for uclass = (aref classes j)
                     do (cond ((or (class= uclass :LRI)
                                   (class= uclass :RLI)
                                   (class= uclass :FSI))
                               (incf depth))
                              ((class= uclass :PDI)
                               (decf depth)
                               (when (= 0 depth)
                                 (setf (aref matching-pdis i) j)
                                 (setf (aref matching-initiator j) i)
                                 (loop-finish)))))
               (when (= -1 (aref matching-pdis i))
                 (setf (aref matching-pdis i) (length classes)))))
    (values matching-pdis matching-initiator)))

(defun determine-paragraph-embedding-level (classes matching-pdis start end)
  (loop for i from start below end
        for class = (aref classes i)
        do (cond ((or (class= class :L))
                  (return 0))
                 ((or (class= class :AL)
                      (class= class :R))
                  (return 1))
                 ((or (class= class :FSI)
                      (class= class :LRI)
                      (class= class :RLI))
                  (setf i (aref matching-pdis i))))
        finally (return 0)))

(defun determine-explicit-embedding-levels (paragraph-level classes matching-pdis)
  (let* ((stack (make-status-stack))
         (overflow-isolates 0)
         (overflow-embeddings 0)
         (valid-isolates 0)
         (result-levels (make-array (length classes) :element-type '(unsigned-byte 8) :initial-element paragraph-level))
         (result-types (copy-seq classes)))
    (declare (dynamic-extent stack))
    (push-status paragraph-level (class-id :ON) NIL stack)
    (loop for i from 0 below (length classes)
          for class = (aref classes i)
          do (cond ((or (class<= :LRE class :RLO)
                        (class<= :LRI class :FSI))
                    (let ((isolate-p (class<= :LRI class :FSI))
                          (rtl-p (if (class= class :FSI)
                                     (= 1 (determine-paragraph-embedding-level classes matching-pdis (1+ i) (aref matching-pdis i)))
                                     (or (class= class :RLE)
                                         (class= class :RLO)
                                         (class= class :RLI)))))
                      (when isolate-p
                        (setf (aref result-levels i) (last-level stack))
                        (unless (class= (last-override stack) :ON)
                          (setf (aref result-types i) (last-override stack))))
                      (let ((new-level (if rtl-p
                                           (logior 1 (1+ (last-level stack)))
                                           (logand (lognot 1) (+ 2 (last-level stack))))))
                        (cond ((and (<= new-level MAX-DEPTH)
                                    (= 0 overflow-isolates)
                                    (= 0 overflow-embeddings))
                               (when isolate-p
                                 (incf valid-isolates))
                               (push-status new-level
                                            (cond ((class= class :LRO)
                                                   (class-id :L))
                                                  ((class= class :RLO)
                                                   (class-id :R))
                                                  (T
                                                   (class-id :ON)))
                                            isolate-p
                                            stack)
                               (unless isolate-p
                                 (setf (aref result-levels i) new-level)))
                              (isolate-p
                               (incf overflow-isolates))
                              ((= 0 overflow-isolates)
                               (incf overflow-embeddings))))))
                   ((class= class :PDI)
                    (cond ((< 0 overflow-isolates)
                           (decf overflow-isolates))
                          ((< 0 valid-isolates)
                           (setf overflow-embeddings 0)
                           (loop until (last-isolate stack)
                                 do (pop-status stack))
                           (pop-status stack)
                           (decf valid-isolates)))
                    (setf (aref result-levels i) (last-level stack)))
                   ((class= class :PDF)
                    (setf (aref result-levels i) (last-level stack))
                    (cond ((< 0 overflow-isolates))
                          ((< 0 overflow-embeddings)
                           (decf overflow-embeddings))
                          ((and (<= 2 (stack-depth stack))
                                (null (last-isolate stack)))
                           (pop-status stack))))
                   ((class= class :B)
                    (setf overflow-isolates 0)
                    (setf overflow-embeddings 0)
                    (setf valid-isolates 0)
                    (setf (aref result-levels i) paragraph-level)
                    ;; Start fresh
                    (empty-stack stack)
                    (push-status paragraph-level (class-id :ON) NIL stack))
                   (T
                    (setf (aref result-levels i) (last-level stack))
                    (unless (class= (last-override stack) :ON)
                      (setf (aref result-types i) (last-override stack))))))
    (values result-types result-levels)))

(defun determine-level-runs (string result-levels)
  (declare (optimize speed))
  (declare (type string string) (type levels result-levels))
  ;; FIXME: this seems very inefficient?
  (let ((temp (make-array (length string) :element-type 'idx))
        (runs (make-array (length string) :fill-pointer 0))
        (level -1)
        (length 0))
    (loop for i from 0 below (length string)
          do (unless (removed-by-x9-p (class-at string i))
               (unless (= level (aref result-levels i))
                 (when (<= 0 level)
                   (vector-push (subseq temp 0 length) runs))
                 (setf level (aref result-levels i))
                 (setf length 0))
               (setf (aref temp length) i)
               (incf length)))
    (when (< 0 length)
      (vector-push (subseq temp 0 length) runs))
    runs))

(defun determine-isolating-run-sequences (string level result-types result-levels matching-pdis matching-initiator)
  (let* ((level-runs (determine-level-runs string result-levels))
         (run-for-char (make-array (length string) :element-type 'idx))
         (sequences (make-array (length level-runs) :fill-pointer 0))
         (current (make-array (length string) :element-type 'idx)))
    (loop for run across level-runs
          for run-i from 0
          do (loop for i from 0 below (length run)
                   for idx = (aref run i)
                   do (setf (aref run-for-char idx) run-i)))
    ;; FIXME: We can avoid allocating the isolatingrunsequence instances altogether
    ;;        and perform the resolutions in here.
    (loop for i from 0
          for run across level-runs
          for first-char = (aref run 0)
          do (when (or (/= (class-at string first-char) (class-id :PDI))
                       (= -1 (aref matching-initiator first-char)))
               (loop with current-length = 0
                     with run = i
                     do (replace current (aref level-runs run) :start1 current-length)
                        (incf current-length (length (aref level-runs run)))
                        (let* ((last-char (aref current (1- current-length)))
                               (last-type (class-at string last-char)))
                          (if (and (or (class= last-type :LRI)
                                       (class= last-type :RLI)
                                       (class= last-type :FSI))
                                   (/= (length string) (aref matching-pdis last-char)))
                              (setf run (aref run-for-char (aref matching-pdis last-char)))
                              (loop-finish)))
                     finally (vector-push (make-isolating-run-sequence (subseq current 0 current-length) string level result-types result-levels) sequences))))
    sequences))

(defun assign-levels-to-characters-removed-by-x9 (string level result-types result-levels)
  (when (removed-by-x9-p (class-at string 0))
    (setf (aref result-types 0) (class-at string 0))
    (setf (aref result-levels 0) level))
  (loop for i from 1 below (length string)
        for class = (class-at string i)
        do (when (removed-by-x9-p class)
             (setf (aref result-types i) class)
             (setf (aref result-levels i) (aref result-levels (1- i))))))

(defun run-algorithm (string level)
  (let ((classes (make-class-array string)))
    (multiple-value-bind (matching-pdis matching-initiator) (determine-matching-isolates classes)
      (when (= 2 level)
        (setf level (determine-paragraph-embedding-level classes matching-pdis 0 (length classes))))
      (multiple-value-bind (result-types result-levels) (determine-explicit-embedding-levels level classes matching-pdis)
        (let ((sequences (determine-isolating-run-sequences string level result-types result-levels matching-pdis matching-initiator)))
          (loop for sequence across sequences
                do (resolve-weak-types sequence)
                   (resolve-paired-brackets sequence string)
                   (resolve-neutral-types sequence)
                   (resolve-implicit-levels sequence)
                   (apply-levels-and-types sequence result-types result-levels))
          (assign-levels-to-characters-removed-by-x9 string level result-types result-levels)
          (values result-levels level))))))

(defun levels (string &key (base-direction :auto) line-breaks start end)
  ;; FIXME: Use START/END
  (let ((start (or start 0))
        (end (or end (length string))))
    (multiple-value-bind (results paragraph-level)
        (run-algorithm string (ecase base-direction
                                (:left-to-right 0)
                                (:right-to-left 1)
                                (:auto 2)))
      (loop for i from 0 below (length results)
            for type = (class-at string i)
            do (when (or (class= type :B)
                         (class= type :S))
                 (setf (aref results i) paragraph-level)
                 (loop for j downfrom (1- i) to 0
                       do (if (whitespace-p (class-at string j))
                              (setf (aref results j) paragraph-level)
                              (loop-finish)))))
      (loop with start = 0
            for limit in (or line-breaks (list (length string)))
            do (loop for j downfrom (1- limit) above start
                     do (if (whitespace-p (class-at string j))
                            (setf (aref results j) paragraph-level)
                            (loop-finish)))
               (setf start limit))
      (values results
              (ecase paragraph-level
                (0 :left-to-right)
                (1 :right-to-left))))))

(defun make-reorder-array (length)
  (let ((reorder (make-array length :element-type 'idx)))
    (dotimes (i length reorder)
      (setf (aref reorder i) i))))

;; FIXME: Function to do this without generating the indices vector (call-in-order)
(defun reorder (levels &key line-breaks indexes)
  (let ((indexes (or indexes (make-reorder-array (length levels)))))
    (loop with start = 0
          for limit in (or line-breaks (list (length levels)))
          do (compute-reordering levels indexes start (- limit start))
             (setf start limit))
    indexes))

(defun index-array-reverse (arr off len)
  (loop for i from 0 below (/ len 2)
        do (rotatef (aref arr (+ off i))
                    (aref arr (+ off len -1 (- i))))))

(defun compute-reordering (levels result &optional (off 0) (len (length levels)))
  (let ((max-level 0))
    ;; FIXME: reorder NSMs
    (loop for i downfrom (+ off len -1) to off
          do (when (< max-level (aref levels i))
               (setf max-level (aref levels i))))
    (loop for level downfrom max-level above 0
          do (loop for i downfrom (+ off len -1) to off
                   do (when (<= level (aref levels i))
                        (let ((seq-end i))
                          (decf i)
                          (loop while (and (<= off i)
                                           (<= level (aref levels i)))
                                do (decf i))
                          (index-array-reverse result (+ i 1) (- seq-end i))))))))

(defun call-in-order (function string &optional levels indexes)
  (let* ((levels (or levels (levels string)))
         (indexes (or indexes (reorder levels))))
    (loop for i from 0 below (length indexes)
          for idx = (aref indexes i)
          for rtl = (oddp (aref levels idx))
          do (if rtl
                 (multiple-value-call function (mirror-at string idx))
                 (funcall function (char string idx) NIL)))))

(defmacro do-in-order ((character manual-mirror string &optional levels indexes) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,character ,manual-mirror)
              ,@body))
       (call-in-order #',thunk ,string ,levels ,indexes))))
