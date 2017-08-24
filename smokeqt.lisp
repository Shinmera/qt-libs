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

(defmethod stage :before ((stage (eql :prepare-sources)) (library smokeqt) &key &allow-other-keys)
  (stage :install-sources (smokegen library)))

(defmethod stage :around ((stage (eql :compile-sources)) (library smokeqt) &key &allow-other-keys)
  (let* ((libdirs (subdirectory (install-directory (smokegen library)) "lib"))
         (ldvar #+(and unix (not darwin)) "LD_LIBRARY_PATH" #+darwin "DYLD_LIBRARY_PATH" #+windows "PATH")
         (ld-orig (get-path ldvar)))
    (set-path (list* (externalize libdirs) (externalize (subdirectory libdirs "smokegen")) ld-orig) ldvar)
    (unwind-protect
         (call-next-method)
      (set-path ld-orig ldvar))))

(defmethod output-files ((library smokeqt))
  (if (probe-file (subdirectory (install-directory library) "lib"))
      (uiop:directory* (merge-pathnames uiop:*wild-file* (subdirectory (install-directory library) "lib")))
      (uiop:directory* (merge-pathnames uiop:*wild-file* (install-directory library)))))
