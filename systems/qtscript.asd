(asdf/parse-defsystem:defsystem #:qtscript
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtscript foreign library."
  :module "QTSCRIPT"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtScript")
               ("qt-libs:foreign-library-component" "smokeqtscript"))
  :depends-on (:qt+libs :qtcore))