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
   #:load-libcommonqt
   #:set-qt-plugin-paths
   #:patch-qt
   #:unpatch-qt))
(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (asdf:system-relative-pathname :qt-libs "standalone" :type :directory))

(defun copy-libs (from to &key (test (constantly T)) force)
  (dolist (input (etypecase from
                   (list from)
                   (pathname (append (uiop:directory-files from)
                                     (uiop:subdirectories from)))))
    (if (uiop:directory-pathname-p input)
        (copy-libs input (relative-dir to (car (last (pathname-directory input)))) :test test :force force)
        (when (funcall test input)
          (let ((output (make-pathname :defaults to
                                       :type (determine-shared-library-type input)
                                       :name (determine-shared-library-name input))))
            (when (or force (not (uiop:file-exists-p output)))
              (ensure-directories-exist output)
              (status 1 "Copying ~s to ~s" (uiop:native-namestring input) (uiop:native-namestring output))
              (uiop:copy-file input output)))))))

(defun ensure-standalone-libs (&key force (standalone-dir *standalone-libs-dir*))
  (let ((dirty force)
        (source-type #-windows :sources #+windows :compiled))
    (flet ((ensure-installed (so system)
             (when (or force (and (not (uiop:file-exists-p (shared-library-file :name so :defaults standalone-dir)))))
               (install-system system :source-type source-type)
               (copy-libs (shared-library-files system) standalone-dir :force force)
               (setf dirty T))))
      (ensure-installed "QtCore" :qt4)
      (ensure-installed "smokebase" :smokegen)
      (ensure-installed "smokeqtcore" :smokeqt)
      (ensure-installed "commonqt" :libcommonqt)
      #+windows (ensure-installed "qtcore" :qt4))
    #+darwin
    (when dirty
      (fix-dylib-collection (uiop:directory-files standalone-dir (make-pathname :type "dylib" :defaults uiop:*wild-path*)))))
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

(defvar *original-funcs* (make-hash-table :test 'eql))

(defun csymb (def)
  (etypecase def
    (list (find-symbol (string (second def)) (string (first def))))
    (symbol def)
    (string (find-symbol def))))

(defun original-func (name)
  (gethash (csymb name) *original-funcs*))

(defun (setf original-func) (func name)
  (setf (gethash (csymb name) *original-funcs*) func))

(defun apply-original-func (name &rest args)
  (apply #'apply
         (or (original-func name)
             (error "SOMETHING PROBABLY WENT HORRIBLY WRONG! I was asked to find the original function to ~s, yet it has not been saved!"
                    name))
         args))

(defun swap-func (original new)
  (let ((original (csymb original))
        (new (csymb new)))
    (unless (original-func original)
      (setf (original-func original)
            (fdefinition original)))
    (unless (eql (fdefinition original) (fdefinition new))
      (unless (eql (fdefinition original) (original-func original))
        (warn "Function definition got changed under our nose!"))
      (status 0 "Swapping out ~s for ~s." original new)
      (setf (fdefinition original) (fdefinition new)))))

(defun restore-func (original)
  (status 0 "Restoring ~s to original definition." original)
  (setf (fdefinition original) (original-func original)))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
    #+windows (pushnew-path *standalone-libs-dir* "PATH")
    #+darwin (pushnew-path *standalone-libs-dir* "DYLD_LIBRARY_PATH")
    #+unix (pushnew-path *standalone-libs-dir* "LD_LIBRARY_PATH")
    ;; See QT::LOAD-LIBCOMMONQT for an explanation of this.
    #+(and sbcl (not windows)) (sb-sys:enable-interrupt sb-unix:sigchld :default)
    ;; Do the loading.
    (cffi:use-foreign-library libsmokebase)
    #+darwin (cffi:use-foreign-library libsmokeqtcore)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))

(defun set-qt-plugin-paths (&rest paths)
  (funcall (csymb '(qt interpret-call)) "QCoreApplication" "setLibraryPaths"
           (mapcar #'uiop:native-namestring paths)))

(defun make-qapplication (&rest args)
  (or (symbol-value (csymb '(qt *qapplication*)))
      (prog1 (apply-original-func '(QT MAKE-QAPPLICATION) args)
        (set-qt-plugin-paths *standalone-libs-dir* (relative-dir *standalone-libs-dir* "plugins")))))

(defun patch-qt ()
  (swap-func '(qt load-libcommonqt) 'load-libcommonqt)
  (swap-func '(qt make-qapplication) 'make-qapplication))

(defun unpatch-qt ()
  (restore-func '(qt load-libcommonqt))
  (restore-func '(qt make-qapplication)))
