#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qt-lib-generator
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bundling of necessary shared object files and build scripts."
  :homepage "https://github.com/Shinmera/qt-libs"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "osx")
               (:file "pathnames")
               (:file "setenv")
               (:file "dylib")
               (:file "archives")
               (:file "build")
               ;(:file "qt4")
               ;(:file "smokegen")
               ;(:file "smokeqt")
               ;(:file "commonqt")
               )
  :depends-on (:trivial-features
               :cl-ppcre
               #+sbcl :sb-posix))
