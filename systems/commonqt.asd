(asdf/parse-defsystem:defsystem #:commonqt
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the commonqt foreign library."
  :serial t
  :components (("qt-libs:foreign-library-component" "QtCore")
               ("qt-libs:foreign-library-component" "QtGui")
               ("qt-libs:foreign-library-component" "commonqt"))
  :depends-on (:qt+libs :smokebase))