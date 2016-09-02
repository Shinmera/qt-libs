#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass libcommonqt (make-build-library github-library checksummed-library)
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
                  (externalize (subdirectory base "lib/"))
                  (externalize (subdirectory base "include/"))))
        (format stream "~&~a" contents))
      file)))

(defmethod stage ((stage (eql :prepare-sources)) (library libcommonqt) &key)
  (check-prerequisite "Qt4.8" "qmake-qt4" "qmake")
  (stage :install-sources (smokeqt library))
  (let ((project-file (merge-pathnames "commonqt.pro" (build-directory library))))
    (fix-commonqt-pro-file project-file
                           (install-directory (smokeqt library))
                           (install-directory (smokegen (smokeqt library))))
    (run-here "`command -v qmake-qt4 || command -v qmake` ~a~s -o ~s"
              #+darwin "-spec macx-g++ " #-darwin ""
              (externalize project-file)
              (externalize (merge-pathnames (make-file library) (build-directory library))))))

(defmethod stage ((stage (eql :install-sources)) (library libcommonqt) &key)
  (copy-file (shared-library-file :name "commonqt" :defaults (build-directory library))
             (install-directory library)))
