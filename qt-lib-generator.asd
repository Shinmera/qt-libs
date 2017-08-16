#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem qt-lib-generator
  :version "2.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bundling of necessary shared object files and build scripts."
  :homepage "https://github.com/Shinmera/qt-libs"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "osx")
               (:file "linux")
               (:file "build")
               (:file "qt4")
               (:file "smokegen")
               (:file "smokeqt")
               (:file "libcommonqt"))
  :depends-on (:trivial-features
               :pathname-utils
               :cl-ppcre
               #+sbcl :sb-posix))
