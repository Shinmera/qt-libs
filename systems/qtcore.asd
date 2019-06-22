(asdf/parse-defsystem:defsystem #:qtcore
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtcore foreign library."
  :module "QTCORE"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtCore")
               ("qt-libs:foreign-library-component" "smokeqtcore"))
  :depends-on (:qt+libs :commonqt))