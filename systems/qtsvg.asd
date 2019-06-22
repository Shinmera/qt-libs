(asdf/parse-defsystem:defsystem #:qtsvg
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtsvg foreign library."
  :module "QTSVG"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtSvg")
               ("qt-libs:foreign-library-component" "smokeqtsvg"))
  :depends-on (:qt+libs :qtcore :qtgui))