#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-9
  (:use #:cl)
  (:shadow #:class)
  (:export
   #:*bidi-class-database-file*
   #:*bidi-brackets-table-file*
   #:*bidi-mirroring-table-file*
   #:load-databases
   #:compile-databases
   #:levels
   #:reorder))
