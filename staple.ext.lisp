(asdf:load-system :staple-markless)

(defpackage "uax-9-docs"
  (:use #:cl)
  (:local-nicknames
   (#:uax-9 #:org.shirakumo.alloy.uax-9)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "uax-9-docs")))

(defmethod staple:subsystems ((system (eql (asdf:find-system :uax-9))))
  ())

(defmethod staple:page-type ((system (eql (asdf:find-system :uax-9))))
  'page*)

(defmethod staple:packages ((system (eql (asdf:find-system :uax-9))))
  (list (find-package (string '#:org.shirakumo.alloy.uax-9))))
