(asdf:defsystem uax-9-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test system for UAX-9."
  :homepage "https://github.com/Shinmera/uax-9"
  :bug-tracker "https://github.com/Shinmera/uax-9/issues"
  :source-control (:git "https://github.com/Shinmera/uax-9.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:uax-9 :parachute :cl-ppcre)
  :perform (asdf:test-op (op c) (uiop:symbol-call :org.shirakumo.alloy.uax-9.test :test)))
