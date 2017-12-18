(asdf/parse-defsystem:defsystem #:qtxmlpatterns
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtxmlpatterns foreign library."
  :module "QTXMLPATTERNS"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtXmlPatterns")
               ("qt-libs:foreign-library-component" "smokeqtxmlpatterns"))
  :depends-on (:qtcore :qtnetwork))