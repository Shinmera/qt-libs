#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass smokeqt (cmake-build-library github-library checksummed-library)
  ((smokegen :initarg :smokegen :initform (make-instance 'smokegen) :accessor smokegen))
  (:default-initargs :tag "qt-libs2.0.1"))

(defmethod cmake-flags ((library smokeqt))
  (list* "-DCMAKE_BUILD_TYPE=Release"
         "-Wno-dev"
         (format NIL "-DCMAKE_INSTALL_PREFIX=~a" (externalize (install-directory library)))
         (format NIL "-DSmoke_DIR=~a" (externalize (subdirectory (install-directory (smokegen library)) "share" "smoke" "cmake")))
         (call-next-method)))

(defmethod stage :before ((stage (eql :prepare-sources)) (library smokeqt) &key)
  (stage :install-sources (smokegen library)))

(defmethod stage :around ((stage (eql :compile-sources)) (library smokeqt) &key)
  (let* ((libdirs (subdirectory (install-directory (smokegen library)) "lib"))
         (ldvar #+linux "LD_LIBRARY_PATH" #+darwin "DYLD_LIBRARY_PATH" #+windows "PATH")
         (ld-orig (get-path ldvar)))
    (set-path (list* (externalize libdirs) (externalize (subdirectory libdirs "smokegen")) ld-orig) ldvar)
    (unwind-protect
         (call-next-method)
      (set-path ld-orig ldvar))))

(defmethod output-files ((library smokeqt))
  (make-shared-library-files
   '("smokephonon"
     "smokeqimageblitz"
     "smokeqsci"
     "smokeqt3support"
     "smokeqtcore"
     "smokeqtdbus"
     "smokeqtdeclarative"
     "smokeqtgui"
     "smokeqthelp"
     "smokeqtmultimedia"
     "smokeqtnetwork"
     "smokeqtopengl"
     "smokeqtscript"
     "smokeqtsql"
     "smokeqtsvg"
     "smokeqttest"
     "smokeqtuitools"
     "smokeqtwebkit"
     "smokeqtxmlpatterns"
     "smokeqtxml"
     "smokeqwt")
   (list (install-directory library)
         (subdirectory (install-directory library) "lib"))))
