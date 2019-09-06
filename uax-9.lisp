#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-9)

(declaim (inline class-at))
(defun class-at (string i)
  (bidi-class (char-code (char string i))))

(defun bidi-string-p (string)
  (declare (optimize speed))
  (declare (type string string))
  (not (loop for char across string
             for class = (bidi-class (char-code char))
             always (or (eql class (class-id :L)) ; Early out
                        (and ; Ensure it's none of the RTL classes.
                         (not (eql class (class-id :R)))
                         (not (eql class (class-id :AL)))
                         (not (eql class (class-id :AN)))
                         (not (<= (class-id :LRE) class (class-id :PDI))))))))

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
          do (when (or (eql class (class-id :LRI))
                       (eql class (class-id :RLI))
                       (eql class (class-id :FSI)))
               (loop with depth = 1
                     for j from (1+ i) below (length classes)
                     for uclass = (aref classes j)
                     do (cond ((or (eql uclass :LRI)
                                   (eql uclass :RLI)
                                   (eql uclass :FSI))
                               (incf depth))
                              ((eql uclass :PDI)
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
        do (cond ((or (eql class (class-id :L)))
                  (return 0))
                 ((or (eql class (class-id :AL))
                      (eql class (class-id :R)))
                  (return 1))
                 ((or (eql class (class-id :FSI))
                      (eql class (class-id :LRI))
                      (eql class (class-id :RLI)))
                  (setf i (aref matching-pdis i))))
        finally (return 0)))

(defun determine-explicit-embedding-levels (level classes matching-pdis)
  (let ((stack (make-status-stack))
        (overflow-isolates 0)
        (overflow-embeddings 0)
        (valid-isolates 0)
        (result-levels (make-array (length classes) :element-type '(unsigned-byte 8) :initial-element level))
        (result-types (make-array (length classes) :element-type '(unsigned-byte 8) :initial-contents classes)))
    (push-status level (class-id :ON) NIL stack)
    (loop for i from 0 below (length classes)
          for class = (aref classes i)
          do (cond ((or (<= (class-id :LRE) class (class-id :RLO))
                        (<= (class-id :LRI) class (class-id :FSI)))
                    (let ((isolate-p (<= (class-id :LRI) class (class-id :FSI)))
                          (rtl-p (if (= class (class-id :FSI))
                                     (= 1 (determine-paragraph-embedding-level classes matching-pdis (1+ i) (aref matching-pdis i)))
                                     (or (= class (class-id :RLE))
                                         (= class (class-id :RLO))
                                         (= class (class-id :RLI))))))
                      (when isolate-p
                        (setf (aref result-levels i) (last-level stack))
                        (unless (= (last-override stack) (class-id :ON))
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
                                            (cond ((= class (class-id :LRO))
                                                   (class-id :L))
                                                  ((= class (class-id :RLO))
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
                   ((= class (class-id :PDI))
                    (cond ((< 0 overflow-isolates)
                           (decf overflow-isolates))
                          ((< 0 valid-isolates)
                           (setf overflow-embeddings 0)
                           (loop until (last-isolate stack)
                                 do (pop-status stack))
                           (pop-status stack)
                           (decf valid-isolates)))
                    (setf (aref result-levels i) (last-level stack)))
                   ((= class (class-id :PDF))
                    (setf (aref result-levels i) (last-level stack))
                    (cond ((< 0 overflow-isolates))
                          ((< 0 overflow-embeddings)
                           (decf overflow-embeddings))
                          ((and (<= 2 (stack-depth stack))
                                (null (last-isolate stack)))
                           (pop-status stack))))
                   ((= class (class-id :B))
                    (empty-stack stack)
                    (setf overflow-isolates 0)
                    (setf overflow-embeddings 0)
                    (setf valid-isolates 0)
                    (setf (aref result-levels i) level))
                   (T
                    (setf (aref result-levels i) (last-level stack))
                    (unless (= (last-override stack) (class-id :ON))
                      (setf (aref result-types i) (last-override stack))))))
    (values result-types result-levels)))

(defun determine-level-runs (string result-levels)
  ;; FIXME: this seems very inefficient?
  (let ((temp (make-array (length string) :element-type 'idx))
        (runs (make-array 128 :adjustable T :fill-pointer 0))
        (level -1)
        (length 0))
    (loop for i from 0 below (length string)
          for class = (class-at string i)
          do (unless (removed-by-x9-p class)
               (unless (= level (aref result-levels i))
                 (when (<= 0 level)
                   (vector-push-extend (subseq temp 0 length) runs))
                 (setf level (aref result-levels i))
                 (setf length 0))
               (setf (aref temp length) i)
               (incf length)))
    (when (< 0 length)
      (vector-push-extend (subseq temp 0 length) runs))
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
                          (if (and (or (eql last-type (class-id :LRI))
                                       (eql last-type (class-id :RLI))
                                       (eql last-type (class-id :FSI)))
                                   (/= (length string) (aref matching-pdis last-char)))
                              (setf run (aref run-for-char (aref matching-pdis last-char)))
                              (loop-finish))))
               (vector-push (make-isolating-run-sequence current string level result-types result-levels) sequences)))
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

(defun run-algorithm (string &optional (level 2))
  (let ((classes (make-class-array string)))
    (multiple-value-bind (matching-pdis matching-initiator) (determine-matching-isolates classes)
      (when (= 2 level)
        (setf level (determine-paragraph-embedding-level classes matching-pdis 0 (length classes))))
      (multiple-value-bind (result-levels result-types) (determine-explicit-embedding-levels level classes matching-pdis)
        (let ((sequences (determine-isolating-run-sequences string level result-types result-levels matching-pdis matching-initiator)))
          (loop for sequence across sequences
                do (resolve-weak-types sequence)
                   (resolve-paired-brackets sequence)
                   (resolve-neutral-types sequence)
                   (resolve-implicit-levels sequence)
                   (apply-levels-and-types sequence result-types result-levels))
          (assign-levels-to-characters-removed-by-x9 string level result-types result-levels)
          result-levels)))))

(defun levels (string level result-levels &optional line-breaks)
  ;; FIXME: turn this into call-with-* style that does not allocate anything and instead
  ;;        interactively asks for line breaks as it scans along
  (let ((results (copy-seq result-levels)))
    (loop for i from 0 below (length results)
          for type = (class-at string i)
          do (when (or (= type (class-id :B))
                       (= type (class-id :S)))
               (setf (aref results i) level)
               (loop for j downfrom (1- i) to 0
                     do (if (whitespace-p (class-at string j))
                            (setf (aref results j) level)
                            (loop-finish)))))
    (loop with start = 0
          for limit in line-breaks
          do (loop for j downfrom (1- limit) above start
                   do (if (whitespace-p (class-at string j))
                          (setf (aref results j) level)
                          (loop-finish)))
             (setf start limit))
    results))

(defun reorder (levels &optional line-breaks)
  (let ((result (make-array (length levels) :element-type 'idx)))
    (loop with start = 0
          for limit in (or line-breaks (list (length levels)))
          for temp-levels = (make-array (- limit start) :element-type '(unsigned-byte 8))
          do (replace temp-levels levels :start1 start :end1 (length temp-levels))
             (let ((temp-order (compute-reordering temp-levels)))
               (loop for j from 0 below (length temp-order)
                     do (setf (aref result (+ start j)) (+ start (aref temp-order j)))))
             (setf start limit))
    result))

(defun compute-reordering (levels)
  (let ((result (make-array (length levels) :element-type 'idx)))
    (loop for i from 0 below (length result)
          do (setf (aref result i) i))
    (let ((highest-level 0)
          (lowest-odd (+ MAX-DEPTH 2)))
      (loop for level across levels
            do (when (< highest-level level)
                 (setf highest-level level))
               (when (and (/= 0 (logand 1 level))
                          (< level lowest-odd))
                 (setf lowest-odd level)))
      (loop for level downfrom highest-level to lowest-odd
            do (loop for i from 0 below (length levels)
                     do (when (<= level (aref levels i))
                          (let ((start i)
                                (limit (1+ i)))
                            (loop while (and (< limit (length levels))
                                             (<= level (aref levels limit)))
                                  do (incf limit))
                            (loop for k downfrom (1- limit)
                                  for j from start
                                  while (< j k)
                                  do (rotatef (aref result j) (aref result k)))
                            (setf i limit)))))
      result)))
