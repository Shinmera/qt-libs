#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-libs
  (:nicknames #:org.shirakumo.qtools.libs)
  (:use #:cl #:qt-lib-generator)
  (:export
   #:*standalone-libs-dir*
   #:ensure-standalone-libs
   #:load-libcommonqt))
(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (asdf:system-relative-pathname :qt-libs "standalone" :type :directory))

(defun copy-libs (from to &key (test (constantly T)))
  (dolist (input (etypecase from
                   (list from)
                   (pathname (uiop:directory-files from))))
    (when (funcall test input)
      (let ((output (make-pathname :defaults to
                                   :type (determine-shared-library-type input)
                                   :name (determine-shared-library-name input))))
        (unless (uiop:file-exists-p output)
          (ensure-directories-exist output)
          (status 1 "Copying ~s to ~s" (uiop:native-namestring input) (uiop:native-namestring output))
          (uiop:copy-file input output))))))

(defun ensure-standalone-libs (&key force (standalone-dir *standalone-libs-dir*))
  (let ((dirty force)
        (source-type #-windows :sources #+windows :compiled))
    (flet ((ensure-installed (so system)
             (when (or force (not (uiop:file-exists-p (shared-library-file :name so :defaults standalone-dir))))
               (install-system system :source-type source-type)
               (copy-libs (shared-library-files (asdf:find-system system)) standalone-dir)
               (setf dirty T))))
      (ensure-installed "QtCore" :qt4)
      (ensure-installed "smokebase" :smokegen)
      (ensure-installed "smokeqtcore" :smokeqt)
      (ensure-installed "commonqt" :libcommonqt)
      #+windows (ensure-installed "qtcore" :qt4))
    #+darwin
    (when dirty
      (dolist (file (uiop:directory-files standalone-dir (make-pathname :type "dylib" :defaults uiop:*wild-file*)))
        (fix-dylib-paths file))))
  standalone-dir)

(cffi:define-foreign-library libsmokebase
  (:windows "smokebase.dll")
  (t (:default "libsmokebase")))

(cffi:define-foreign-library libsmokeqtcore
  (:windows "smokeqtcore.dll")
  (t (:default "libsmokeqtcore")))

(cffi:define-foreign-library libcommonqt
  (:windows "commonqt.dll")
  (t (:default "libcommonqt")))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
    (pushnew-path *standalone-libs-dir*)
    ;; See QT::LOAD-LIBCOMMONQT for an explanation of this.
    #+(and sbcl (not windows)) (sb-sys:enable-interrupt sb-unix:sigchld :default)
    ;; Do the loading.
    (cffi:use-foreign-library libsmokebase)
    #+darwin (cffi:use-foreign-library libsmokeqtcore)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))
