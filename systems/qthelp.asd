(asdf/parse-defsystem:defsystem #:qthelp
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qthelp foreign library."
  :module "QTHELP"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtCLucene")
               ("qt-libs:foreign-library-component" "QtHelp")
               ("qt-libs:foreign-library-component" "smokeqthelp"))
  :depends-on (:qtcore :qtgui :qtnetwork :qtsql))