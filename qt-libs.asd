#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem qt-libs
  :version "2.0.2"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "System to ensure that the necessary Qt libs are available."
  :homepage "https://Shinmera.github.io/qt-libs/"
  :bug-tracker "https://github.com/Shinmera/qt-libs/issues"
  :source-control (:git "https://github.com/Shinmera/qt-libs.git")
  :serial T
  :components ((:file "qt-libs"))
  :depends-on (:qt-lib-generator
               :cl-ppcre
               :cffi)
  :perform (asdf:load-op :after (op c) (uiop:symbol-call :qt-libs :ensure-standalone-libs)))
