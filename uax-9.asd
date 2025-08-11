(asdf:defsystem uax-9
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Implementation of the Unicode Standards Annex #9's bidirectional text algorithm"
  :homepage "https://shinmera.com/project/uax-9"
  :bug-tracker "https://shinmera.com/project/uax-9/issues"
  :source-control (:git "https://shinmera.com/project/uax-9.git")
  :serial T
  :components ((:file "package")
               (:file "database")
               (:file "status-stack")
               (:file "isolating-run-sequence")
               (:file "uax-9")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :uax-9-test))))
