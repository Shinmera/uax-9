#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-9)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults
                                (or *compile-file-truename* *load-truename*
                                    (error "COMPILE-FILE or LOAD this file."))))

(defvar *bidi-class-database-file* (make-pathname :name "BidiClass" :type "dat" :defaults *here*))
(defvar *bidi-brackets-table-file* (make-pathname :name "BidiBrackets" :type "dat" :defaults *here*))
(defvar *bidi-mirroring-table-file* (make-pathname :name "BidiMirroring" :type "dat" :defaults *here*))

(defconstant MAX-DEPTH 125)
(defconstant MAX-PAIRING-DEPTH 63)

(deftype class ()
  `(integer 0 23))

(deftype idx ()
  '(integer 0 #.ARRAY-DIMENSION-LIMIT))

(deftype code ()
  '(integer 0 #x10FFFF))

(deftype level ()
  '(integer 0 125))

(deftype classes ()
  `(simple-array (unsigned-byte 8) (*)))

(deftype levels ()
  `(simple-array (unsigned-byte 8) (*)))

(deftype stack ()
  `(simple-array (unsigned-byte 32) (128)))

(defun read-u32le (stream)
  (logior
   (ash (read-byte stream) 0)
   (ash (read-byte stream) 8)
   (ash (read-byte stream) 16)
   (ash (read-byte stream) 24)))

(defmacro defglobal (name value)
  #+sbcl `(sb-ext:defglobal ,name ,value)
  #-sbcl `(defvar ,name ,value))

(declaim (type (simple-array keyword) +bidi-class-list+))
(defglobal +bidi-class-list+
    #(:L      ; Left_To_Right                 any strong left-to-right character
      :R      ; Right_To_Left                 any strong right-to-left (non-Arabic-type) character
      :AL     ; Arabic_Letter                 any strong right-to-left (Arabic-type) character
      ;; Weak Types
      :EN     ; European_Number               any ASCII digit or Eastern Arabic-Indic digit
      :ES     ; European_Separator            plus and minus signs
      :ET     ; European_Terminator           a terminator in a numeric format context, includes currency signs
      :AN     ; Arabic_Number                 any Arabic-Indic digit
      :CS     ; Common_Separator              commas, colons, and slashes
      :NSM    ; Nonspacing_Mark               any nonspacing mark
      :BN     ; Boundary_Neutral              most format characters, control codes, or noncharacters
      ;; Neutral Types
      :B      ; Paragraph_Separator           various newline characters
      :S      ; Segment_Separator             various segment-related control codes
      :WS     ; White_Space                   spaces
      :ON     ; Other_Neutral                 most other symbols and punctuation marks
      ;; Explicit Formatting Types
      :LRE	; Left_To_Right_Embedding	U+202A: the LR embedding control
      :LRO	; Left_To_Right_Override	U+202D: the LR override control
      :RLE	; Right_To_Left_Embedding	U+202B: the RL embedding control
      :RLO	; Right_To_Left_Override	U+202E: the RL override control
      :PDF	; Pop_Directional_Format	U+202C: terminates an embedding or override control
      :LRI	; Left_To_Right_Isolate	        U+2066: the LR isolate control
      :RLI	; Right_To_Left_Isolate	        U+2067: the RL isolate control
      :FSI	; First_Strong_Isolate	        U+2068: the first strong isolate control
      :PDI	; Pop_Directional_Isolate	U+2069: terminates an isolate control
      ))

(declaim (inline class-id))
(defun class-id (class)
  (the (unsigned-byte 8) (position class +bidi-class-list+)))

(define-compiler-macro class-id (&whole whole class &environment env)
  (if (constantp class env)
      `(load-time-value (position ,class +bidi-class-list+))
      whole))

(defun class-ids (&rest classes)
  (mapcar #'class-id classes))

(define-compiler-macro class-ids (&whole whole &rest classes &environment env)
  (if (loop for class in classes always (constantp class env))
      `(load-time-value (list ,@(loop for class in classes collect `(class-id ,class))))
      whole))

(declaim (type (simple-array (unsigned-byte 8) (#x110000)) +bidi-class-map+))
(defglobal +bidi-class-map+ (make-array #x110000 :element-type '(unsigned-byte 8) :initial-element 0))

(defun load-bidi-class-database (&optional (source *bidi-class-database-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +bidi-class-map+))
          do (setf i (read-sequence +bidi-class-map+ stream :start i)))
    +bidi-class-map+))

(declaim (inline bidi-class))
(defun bidi-class (id)
  (aref +bidi-class-map+ id))

(declaim (type hash-table +bidi-brackets-map+))
(defglobal +bidi-brackets-map+ (make-hash-table :test 'eq :size 120))

(defun load-bidi-brackets-table (&optional (source *bidi-brackets-table-file*))
  (clrhash +bidi-brackets-map+)
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop repeat (read-u32le stream)
          for key = (read-u32le stream)
          for val = (read-u32le stream)
          do (setf (gethash key +bidi-brackets-map+) val))
    +bidi-brackets-map+))

(declaim (inline bracket-sibling))
(defun bracket-sibling (id)
  (logand (the (unsigned-byte 32) (gethash id +bidi-brackets-map+ 0)) #xFFFFFF))

(declaim (inline bracket-type))
(defun bracket-type (id)
  (ash (the (unsigned-byte 32) (gethash id +bidi-brackets-map+ 0)) -25))

(declaim (type hash-table +bidi-mirroring-map+))
(defglobal +bidi-mirroring-map+ (make-hash-table :test 'eq :size 420))

(defun load-bidi-mirroring-table (&optional (source *bidi-mirroring-table-file*))
  (clrhash +bidi-mirroring-map+)
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop repeat (read-u32le stream)
          for key = (read-u32le stream)
          for val = (read-u32le stream)
          do (setf (gethash key +bidi-mirroring-map+) val))
    +bidi-mirroring-map+))

(declaim (inline mirror))
(defun mirror (id)
  (the (unsigned-byte 32) (gethash id +bidi-mirroring-map+ id)))

(define-condition no-database-files (warning)
  () (:report (lambda (c s) (format s "Database files are not available.~%Please run ~s" 'compile-databases))))

(defun load-databases ()
  (restart-case
      (cond ((and (probe-file *bidi-class-database-file*)
                  (probe-file *bidi-brackets-table-file*)
                  (probe-file *bidi-mirroring-table-file*))
             (load-bidi-class-database)
             (load-bidi-brackets-table)
             (load-bidi-mirroring-table)
             T)
            (T
             (error 'no-database-files)))
    (compile ()
      :report "Compile the databases and retry."
      (compile-databases))
    (abort ()
      :report "Ignore the missing files. UAX-9 will not work correctly."
      NIL)))

(defun compile-databases ()
  (load (make-pathname :name "compile" :type "lisp" :defaults *here*))
  (funcall (find-symbol (string 'compile-databases) '#:org.shirakumo.alloy.uax-9.compiler))
  (load-databases))

(unless (find '#:uax-9-no-load *features* :test #'string=)
  (handler-case
      (handler-case (load-databases)
        (no-database-files ()
          (format T "~&UAX-9: Database files unavailable, compiling...~%")
          (compile-databases)))
    (error (e)
      (format T "~&UAX-9: ~a" e))))

(declaim (inline code-at))
(defun code-at (string i)
  (char-code (char string i)))

(declaim (inline class-at))
(defun class-at (string i)
  (bidi-class (code-at string i)))

(declaim (inline bracket-sibling-at))
(defun bracket-sibling-at (string i)
  (bracket-sibling (code-at string i)))

(declaim (inline bracket-type-at))
(defun bracket-type-at (string i)
  (bracket-type (code-at string i)))

(declaim (inline mirror-at))
(defun mirror-at (string i)
  (let ((mirror (mirror (code-at string i))))
    (values (code-char (logand #x11FFFF mirror)) (logbitp 31 mirror))))

(defun class= (class expected)
  (= class (class-id expected)))

(define-compiler-macro class= (class expected)
  `(= ,class (class-id ,expected)))

(defun class<= (left class right)
  (<= (class-id left) class (class-id right)))

(define-compiler-macro class<= (left class right)
  `(<= (class-id ,left) ,class (class-id ,right)))

(declaim (inline whitespace-p))
(defun whitespace-p (class)
  (or (class= class :BN)
      (class= class :WS)
      (class<= :LRE class :PDI)))

(declaim (inline removed-by-x9-p))
(defun removed-by-x9-p (class)
  (or (class= class :BN)
      (class<= :LRE class :PDF)))

(defun normalize-strong-type-n0 (code)
  (cond ((class= code :L)
         (class-id :L))
        ((or (class= code :R)
             (class= code :EN)
             (class= code :AN)
             (class= code :AL))
         (class-id :R))
        (T
         (class-id :ON))))

(declaim (inline type-for-level))
(defun type-for-level (level)
  (if (evenp level)
      (class-id :L)
      (class-id :R)))

(declaim (inline neutral-type-p))
(defun neutral-type-p (type)
  (or (class<= :B type :ON)
      (class<= :LRI type :PDI)))
