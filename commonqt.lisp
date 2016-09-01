#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass commonqt (make-build-library checksummed-library cached-build-library)
  ((smokeqt :initarg :smokeqt :initform (make-instance 'smokeqt) :accessor smokeqt))
  (:default-initargs :tag "qt-libs2.0.0"))

(defun fix-commonqt-pro-file (file &rest basepaths)
  (let ((contents (uiop:read-file-string file)))
    (unless (search "Qtools Fix" contents)
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :error)
        (format stream "~&# Qtools fix~%")
        (dolist (base basepaths)
          (format stream "~&LIBS += -L~s~
                          ~&INCLUDEPATH += ~s"
                  (uiop:native-namestring (relative-dir base "lib/"))
                  (uiop:native-namestring (relative-dir base "include/"))))
        (format stream "~&~a" contents))
      file)))

(defmethod prepare-sources ((library commonqt) at)
  (let ((project-file (merge-pathnames "commonqt.pro" at)))
    (fix-commonqt-pro-file project-file
                           (install-dir (smokeqt library))
                           (install-dir (smokegen (smokeqt library))))
    (run-here "`command -v qmake-qt4 || command -v qmake` ~a~s -o ~s"
              #+darwin "-spec macx-g++ " #-darwin ""
              (uiop:native-namestring project-file)
              (uiop:native-namestring (merge-pathnames (make-file library) at)))))

(defmethod install-sources ((library commonqt) at to)
  (copy-file (shared-library-file :name "commonqt") to))
