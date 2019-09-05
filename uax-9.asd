#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem uax-9
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Implementation of the Unicode Standards Annex #9's bidirectional text algorithm"
  :homepage "https://github.com/Shinmera/uax-9"
  :bug-tracker "https://github.com/Shinmera/uax-9/issues"
  :source-control (:git "https://github.com/Shinmera/uax-9.git")
  :serial T
  :components ((:file "package")
               (:file "database")
               (:file "status-stack")
               (:file "isolating-run-sequence")
               (:file "uax-9")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :uax-9-test))))
