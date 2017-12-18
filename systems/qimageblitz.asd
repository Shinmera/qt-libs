(asdf/parse-defsystem:defsystem #:qimageblitz
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qimageblitz foreign library."
  :module "QIMAGEBLITZ"
  :serial t
  :components (("qt-libs:foreign-library-component" "qimageblitz")
               ("qt-libs:foreign-library-component" "smokeqimageblitz"))
  :depends-on (:qtcore :qtgui))