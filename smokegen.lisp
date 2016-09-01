#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass smokegen (cmake-build-library checksummed-library cached-build-library)
  ()
  (:default-initargs :tag "qt-libs2.0.0"))

(defmethod cmake-flags ((library smokegen))
  (list* "-DCMAKE_BUILD_TYPE=Release"
         (format NIL "-DCMAKE_INSTALL_PREFIX=~s" (install-directory library))
         (call-next-method)))

(defmethod install-sources ((library make-build-library) at to)
  (with-chdir (dir)
    (run-here "make install -f ~s" (make-file library)))
  #+darwin
  ;; OS X El Capitan breaks DYLD_LIBRARY_PATH, so we need to fix the binary up.
  (dylib-set-dependency-name
   (merge-pathnames "bin/smokegen" to)
   "libcppparser.dylib" "@executable_path/../lib/libcppparser.dylib")
  )


