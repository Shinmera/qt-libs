(asdf/parse-defsystem:defsystem #:qtsql
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtsql foreign library."
  :module "QTSQL"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtSql")
               ("qt-libs:foreign-library-component" "smokeqtsql"))
  :depends-on (:qt+libs :qtcore :qtgui))