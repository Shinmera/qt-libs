(asdf/parse-defsystem:defsystem #:qt3support
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qt3support foreign library."
  :module "QT3SUPPORT"
  :serial t
  :components (("qt-libs:foreign-library-component" "Qt3Support")
               ("qt-libs:foreign-library-component" "smokeqt3support"))
  :depends-on (:qt+libs :qtcore :qtgui :qtxml :qtnetwork :qtsql))