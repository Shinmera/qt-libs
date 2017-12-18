(asdf/parse-defsystem:defsystem #:qtuitools
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtuitools foreign library."
  :module "QTUITOOLS"
  :serial t
  :components (("qt-libs:foreign-library-component" "smokeqtuitools"))
  :depends-on (:qtcore :qtgui))