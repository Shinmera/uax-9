#|
 This file is a part of UAX-9
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem uax-9-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test system for UAX-9."
  :homepage "https://github.com/Shinmera/uax-9"
  :bug-tracker "https://github.com/Shinmera/uax-9/issues"
  :source-control (:git "https://github.com/Shinmera/uax-9.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:uax-9 :parachute :cl-ppcre)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.alloy.uax-9.test)))
