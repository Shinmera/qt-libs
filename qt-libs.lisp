#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-libs
  (:nicknames #:org.shirakumo.qtools.libs)
  (:use #:cl)
  (:export
   #:*standalone-libs-dir*
   #:ensure-standalone-libs
   #:load-libcommonqt))
(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (ensure-directories-exist
                               (asdf:system-relative-pathname :qt-libs "standalone" :type :directory)))

(defun determine-so-type (pathname)
  (cond ((search ".so." (pathname-name pathname))
         "so")
        (T (pathname-type pathname))))

(defun determine-so-name (pathname)
  (cond ((search ".so." (pathname-name pathname))
         (subseq (pathname-name pathname) 0 (search ".so." (pathname-name pathname))))
        (T
         (or (cl-ppcre:register-groups-bind (name) ("^(.+)\\.\\d\\.\\d\\.\\d$" (pathname-name pathname)) name)
             (pathname-name pathname)))))

(defun copy-libs (from to &key (test (constantly T)))
  (dolist (input (etypecase from
                   (list from)
                   (pathname (uiop:directory-files from))))
    (when (and (or (find (pathname-type input) '("dylib" "dll" "so") :test #'string=)
                   (search ".so." (pathname-name input)))
               (funcall test input))
      (let ((output (make-pathname :defaults to
                                   :type (determine-so-type input)
                                   :name (determine-so-name input))))
        (unless (uiop:file-exists-p output)
          (uiop:copy-file input output))))))

(defun so-file (name defaults)
  (qt-lib-generator:shared-library-file :name name :defaults defaults))

(defun ensure-standalone-libs (&key force (standalone-dir *standalone-libs-dir*))
  (let ((dirty force))
    (when (or force (not (uiop:file-exists-p (so-file "smokebase" standalone-dir))))
      (asdf:compile-system :smokegen)
      (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :smokegen)) standalone-dir
                 :test (lambda (file) (search "smokebase" (pathname-name file))))
      (setf dirty T))
    (when (or force (not (uiop:file-exists-p (so-file "smokeqtgui" standalone-dir))))
      (asdf:compile-system :smokeqt)
      (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :smokeqt)) standalone-dir
                 :test (lambda (file) (search "smoke" (pathname-name file))))
      (setf dirty T))
    (when (or force (not (uiop:file-exists-p (so-file "commonqt" standalone-dir))))
      (asdf:compile-system :libcommonqt)
      (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :libcommonqt)) standalone-dir
                 :test (lambda (file) (search "commonqt" (pathname-name file))))
      (setf dirty T))
    #+darwin
    (when dirty
      (dolist (file (uiop:directory-files standalone-dir))
        (qt-lib-generator:fix-dylib-paths file))))
  standalone-dir)

(cffi:define-foreign-library libsmokebase
  (:windows "smokebase.dll")
  (t (:default "libsmokebase")))

(cffi:define-foreign-library libsmokeqtcore
  (:windows "smokeqtcore.dll")
  (t (:default "libsmokeqtcore")))

(cffi:define-foreign-library libcommonqt
  (:linux (:or "libcommonqt.so.1.0.0" "libcommonqt.so.1" "libcommonqt.so"))
  (:windows "commonqt.dll")
  (t (:default "libcommonqt")))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
    ;; See QT::LOAD-LIBCOMMONQT for an explanation of this.
    #+(and sbcl (not windows)) (sb-sys:enable-interrupt sb-unix:sigchld :default)
    ;; Do the loading.
    (cffi:use-foreign-library libsmokebase)
    #+darwin (cffi:use-foreign-library libsmokeqtcore)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))
