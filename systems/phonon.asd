(asdf/parse-defsystem:defsystem #:phonon
  :defsystem-depends-on (:qt-libs)
  :class "qt-libs:foreign-library-system"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Loads the phonon foreign library."
  :module "PHONON"
  :serial t
  :components (("qt-libs:foreign-library-component" "phonon")
               ("qt-libs:foreign-library-component" "smokephonon"))
  :depends-on (:qtcore :qtgui (:feature (:or :linux :darwin) :qtdbus)
               (:feature (:or :linux :darwin) :qtxml)))