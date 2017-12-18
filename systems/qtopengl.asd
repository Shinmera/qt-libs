(asdf/parse-defsystem:defsystem #:qtopengl
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtopengl foreign library."
  :module "QTOPENGL"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtOpenGL")
               ("qt-libs:foreign-library-component" "smokeqtopengl"))
  :depends-on (:qtcore :qtgui))