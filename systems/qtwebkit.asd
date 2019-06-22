(asdf/parse-defsystem:defsystem #:qtwebkit
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtwebkit foreign library."
  :module "QTWEBKIT"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtWebKit")
               ("qt-libs:foreign-library-component" "smokeqtwebkit"))
  :depends-on (:qt+libs :qtcore :qtgui :qtnetwork))