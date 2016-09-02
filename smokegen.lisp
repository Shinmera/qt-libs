#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass smokegen (cmake-build-library github-library checksummed-library)
  ()
  (:default-initargs :tag "qt-libs2.0.0"))

(defmethod cmake-flags ((library smokegen))
  (list* "-DCMAKE_BUILD_TYPE=Release"
         "-Wno-dev"
         (format NIL "-DCMAKE_INSTALL_PREFIX=~a" (externalize (install-directory library)))
         (call-next-method)))

(defmethod stage :after ((stage (eql :install)) (library smokegen) &key)
  #+darwin
  ;; OS X El Capitan breaks DYLD_LIBRARY_PATH, so we need to fix the binary up.
  (dylib-set-dependency-name
   (merge-pathnames "bin/smokegen" to)
   "libcppparser.dylib" "@executable_path/../lib/libcppparser.dylib"))

(defmethod output-files ((library smokegen))
  (make-shared-library-files
   '("cppparser")
   (list (install-directory library)
         (subdirectory (install-directory library) "lib"))))
