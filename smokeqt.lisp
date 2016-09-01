#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass smokeqt (cmake-build-library checksummed-library cached-build-library)
  ((smokegen :initarg :smokegen :initform (make-instance 'smokegen) :accessor smokegen))
  (:default-initargs :tag "qt-libs2.0.0"))

(defmethod cmake-flags ((library smokeqt))
  (list* "-DCMAKE_BUILD_TYPE=Release"
         "-Wno-dev"
         (format NIL "-DCMAKE_INSTALL_PREFIX=~s" (install-directory library))
         (format NIL "-DSmoke_DIR=~s" (relative-dir (install-directory (smokegen library)) "share" "smoke" "cmake"))
         (call-next-method)))

(defmethod compile-sources ((library smokeqt) at)
  (let* ((libdirs (relative-dir (install-directory (smokegen library)) "lib"))
         (*ld-library-path* (list* (uiop:native-namestring libdirs)
                                   (uiop:native-namestring (relative-dir libdirs "smokeqt"))
                                   *ld-library-path*)))
    (call-next-method)))
