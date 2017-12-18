(asdf/parse-defsystem:defsystem #:qtxml
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtxml foreign library."
  :module "QTXML"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtXml")
               ("qt-libs:foreign-library-component" "smokeqtxml"))
  :depends-on (:qtcore))