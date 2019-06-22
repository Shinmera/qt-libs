(asdf/parse-defsystem:defsystem #:qwt
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qwt foreign library."
  :module "QWT"
  :serial t
  :components (("qt-libs:foreign-library-component" "qwt")
               ("qt-libs:foreign-library-component" "smokeqwt"))
  :depends-on (:qt+libs :qtcore :qtgui (:feature :windows :qtsvg)))