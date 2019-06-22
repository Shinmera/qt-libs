(asdf/parse-defsystem:defsystem #:smokebase
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the smokebase foreign library."
  :serial t
  :components (("qt-libs:foreign-library-component" "smokebase"))
  :depends-on (:qt+libs))