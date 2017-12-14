#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-libs
  (:nicknames #:org.shirakumo.qtools.libs)
  (:use #:cl #:qt-lib-generator #:pathname-utils)
  (:export
   #:*standalone-libs-dir*
   #:ensure-standalone-libs
   #:ensure-lib-loaded
   #:load-libcommonqt
   #:set-qt-plugin-paths
   #:fix-qt-plugin-paths
   #:patch-qt
   #:unpatch-qt
   #:setup-paths))
(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (asdf:system-relative-pathname :qt-libs "standalone" :type :directory))

(defun starts-with (a string)
  (and (<= (length a) (length string))
       (string= a string :end2 (length a))))

(defun normalize-library-name (name)
  (when (starts-with "lib" name)
    (setf name (subseq name (length "lib"))))
  (when (starts-with "qtlibs!" name)
    (setf name (subseq name (length "qtlibs!"))))
  name)

(defun installed-library-file (name &optional (defaults *standalone-libs-dir*))
  #-(or windows unix) (error "Don't know how to create shared library files on your OS.")
  (make-pathname :name #+unix (format NIL "qtlibs!~a" (normalize-library-name name))
                       #+windows (if (find name qt-lib-generator::*qt-module-list* :test #'string-equal)
                                     (format NIL "~a4" name)
                                     name)
                 :type #+windows "dll" #+darwin "dylib" #+(and unix (not darwin)) "so"
                 :defaults defaults))

(defun copy-directory-tree (from to &key force)
  (dolist (file (append (uiop:directory-files from)
                        (uiop:subdirectories from)) to)
    (let ((output (make-pathname :name (pathname-name file)
                                 :type (pathname-type file)
                                 :defaults to)))
      (cond ((directory-p file)
             (copy-directory-tree
              file
              (subdirectory to (directory-name file))
              :force force))
            ((or force (not (uiop:file-exists-p output)))
             (ensure-directories-exist output)
             (status 1 "Copying ~s to ~s" (uiop:native-namestring file) (uiop:native-namestring output))
             (copy-file file output))))))

(defun copy-libs (from to &key (test (constantly T)) force)
  (dolist (input (etypecase from
                   (list from)
                   (pathname (append (uiop:directory-files from)
                                     (uiop:subdirectories from)))))
    (if (uiop:directory-pathname-p input)
        ;; KLUDGE: Special handling for the Qt plugins.
        (if (find "plugins" (pathname-directory input) :test #'string=)
            (copy-directory-tree input (subdirectory to (directory-name input)) :force force)
            (copy-libs input (subdirectory to (directory-name input)) :test test :force force))
        (when (funcall test input)
          (let ((output (installed-library-file (determine-shared-library-name input) to)))
            (when (or force (not (uiop:file-exists-p output)))
              (ensure-directories-exist output)
              (status 1 "Copying ~s to ~s" (uiop:native-namestring input) (uiop:native-namestring output))
              (uiop:copy-file input output)))))))

(defun ensure-standalone-libs (&key (method :install-binaries) force (standalone-dir *standalone-libs-dir*))
  (let ((dirty force))
    (flet ((ensure-installed (so system)
             (when (or force (and (not (uiop:file-exists-p (installed-library-file so standalone-dir)))))
               (let ((system (make-instance system)))
                 (when method (stage method system :force force))
                 (copy-libs (output-files system) standalone-dir :force force)
                 (setf dirty T)))))
      (ensure-installed "QtCore" 'qt4)
      (ensure-installed "smokebase" 'smokegen)
      (ensure-installed "smokeqtcore" 'smokeqt)
      (ensure-installed "commonqt" 'libcommonqt))
    (when (and dirty (eql method :install-sources))
      #+darwin (fix-dylib-collection (uiop:directory-files standalone-dir (make-pathname :type "dylib" :defaults uiop:*wild-path*)))
      #+linux (fix-ldlib-collection (uiop:directory-files standalone-dir (make-pathname :type "so" :defaults uiop:*wild-path*)))))
  standalone-dir)

(defun ensure-lib-loaded (file &optional name)
  (let ((file (etypecase file
                (pathname file)
                (string (installed-library-file file))))
        (name (or name (intern (string-upcase (pathname-name file)))))
        #+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    (cffi::register-foreign-library
     name `((T ,file))
     :search-path (to-directory file))
    (unless (cffi:foreign-library-loaded-p name)
      (cffi:load-foreign-library name))))

(defun setup-paths ()
  (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
  #+windows (pushnew-path *standalone-libs-dir* "PATH")
  #+darwin (pushnew-path *standalone-libs-dir* "DYLD_LIBRARY_PATH")
  #+unix (pushnew-path *standalone-libs-dir* "LD_LIBRARY_PATH"))
(setup-paths)
