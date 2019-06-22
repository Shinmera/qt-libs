(asdf/parse-defsystem:defsystem #:qtnetwork
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the qtnetwork foreign library."
  :module "QTNETWORK"
  :serial t
  :components (("qt-libs:foreign-library-component" "QtNetwork")
               ("qt-libs:foreign-library-component" "smokeqtnetwork"))
  :depends-on (:qt+libs :qtcore))