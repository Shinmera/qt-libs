(asdf/parse-defsystem:defsystem #:qsci
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qsci foreign library."
  :module "QSCI"
  :serial t
  :components (("qt-libs:foreign-library-component" "qscintilla2")
               ("qt-libs:foreign-library-component" "smokeqsci"))
  :depends-on (:qt+libs :qtcore :qtgui))