(asdf/parse-defsystem:defsystem #:qtdbus
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtdbus foreign library."
  :module "QTDBUS"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtDBus")
               ("qt-libs:foreign-library-component" "smokeqtdbus"))
  :depends-on (:qtcore :qtxml))