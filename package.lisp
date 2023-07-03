(defpackage #:org.shirakumo.alloy.uax-9
  (:use #:cl)
  (:shadow #:class)
  (:export
   #:*bidi-class-database-file*
   #:*bidi-brackets-table-file*
   #:*bidi-mirroring-table-file*
   #:no-database-files
   #:load-databases
   #:compile-databases
   #:mirror-at
   #:levels
   #:reorder
   #:call-in-order
   #:do-in-order))
