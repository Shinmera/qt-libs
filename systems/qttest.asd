(asdf/parse-defsystem:defsystem #:qttest
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qttest foreign library."
  :module "QTTEST"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtTest")
               ("qt-libs:foreign-library-component" "smokeqttest"))
  :depends-on (:qtcore :qtgui))