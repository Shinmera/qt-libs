(asdf/parse-defsystem:defsystem #:qtdeclarative
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtdeclarative foreign library."
  :module "QTDECLARATIVE"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtDeclarative")
               ("qt-libs:foreign-library-component" "smokeqtdeclarative"))
  :depends-on (:qtcore :qtgui :qtnetwork :qtscript :qtsql :qtxmlpatterns))