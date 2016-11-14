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

(defun normalize-library-name (name)
  (if (and (< 3 (length name)) (string= "lib" name :end2 3))
      (subseq name 3)
      name))

(defun installed-library-file (name &optional (defaults *standalone-libs-dir*))
  (make-pathname :name #-linux name #+linux (format NIL "qtlibs!~a" (normalize-library-name name))
                 :type #+windows "dll" #+darwin "dylib" #+linux "so"
                 :defaults defaults))

(defun copy-directory-tree (from to &key force)
  (dolist (file (append (uiop:directory-files from)
                        (uiop:subdirectories from)) to)
    (let ((output (make-pathname :name (pathname-name file)
                                 :type (pathname-type file)
                                 :defaults to)))
      (cond ((pathname-utils:directory-p file)
             (copy-directory-tree
              file
              (pathname-utils:subdirectory to (pathname-utils:directory-name file))
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
            (copy-directory-tree input to :force force)
            (copy-libs input (subdirectory to (car (last (pathname-directory input)))) :test test :force force))
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
      #+linux (fix-ldlib-collection (uiop:directory-files standalone-dir (make-pathname :type "so" :defaults uiop:*wild-path*)))
      #+windows T))
  standalone-dir)

(defvar *original-funcs* (make-hash-table :test 'eql))

(defun csymb (def)
  (etypecase def
    (list (find-symbol (string (second def)) (string (first def))))
    (symbol def)
    (string (find-symbol def))))

(defmacro qtcall (symb &rest args)
  `(funcall (csymb '(qt ,symb)) ,@args))

(defmacro qtvar (symb)
  `(symbol-value (csymb '(qt ,symb))))

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

(defun ensure-lib-loaded (file &optional name)
  (let ((file (etypecase file
                (pathname file)
                (string (installed-library-file file))))
        (name (or name (intern (string-upcase (pathname-name file)))))
        #+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    (cffi::register-foreign-library
     name `((T ,file))
     :search-path (pathname-utils:to-directory file))
    (unless (cffi:foreign-library-loaded-p name)
      (cffi:load-foreign-library name))))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    ;; See QT::LOAD-LIBCOMMONQT for an explanation of this.
    #+(and sbcl (not windows)) (sb-sys:enable-interrupt sb-unix:sigchld :default)
    ;; Do the loading.
    #+linux (ensure-lib-loaded "audio")
    (ensure-lib-loaded #-windows "QtCore" #+windows "QtCore4")
    (ensure-lib-loaded #-windows "QtGui" #+windows "QtGui4")
    (ensure-lib-loaded "smokebase")
    (ensure-lib-loaded "smokeqtcore")
    (ensure-lib-loaded "smokeqtgui")
    (ensure-lib-loaded "commonqt")
    (when (find-package :qt) (setf (symbol-value (find-symbol (string '*LIBRARY-LOADED-P*) :QT)) T))
    (setf *libs-loaded* T)))

(defun set-qt-plugin-paths (&rest paths)
  (qtcall interpret-call "QCoreApplication" "setLibraryPaths"
          (mapcar #'uiop:native-namestring paths)))

(defun fix-qt-plugin-paths (&optional (base *standalone-libs-dir*))
  (set-qt-plugin-paths base (subdirectory base "plugins")))

(defun make-qapplication (&rest args)
  (or (qtvar *qapplication*)
      (prog1 (apply-original-func '(QT MAKE-QAPPLICATION) args)
        (fix-qt-plugin-paths))))

;; We only override this to replace the explicit file loading by our
;; ensure-lib-loaded function. Everything else is semantically the same
;; but we have to use delegated symbol resolving as this file is loaded
;; before the QT package exists, hence the usage of QTCALL and QTVAR
;; where necessary.
(defun ensure-smoke (name)
  (qtcall ensure-loaded)
  (let ((name (string-downcase name)))
    (unless (qtcall named-module-number name)
      (let ((idx (qtvar *n-modules*)))
        (unless (< idx (length (qtvar *module-table*)))
          (error "Sorry, +module-bits+ exceeded"))
        (ensure-lib-loaded (format NIL "smoke~a" name))
        (let ((init (cffi:foreign-symbol-pointer
                     (format nil "init_~A_Smoke" name))))
          (assert init)
          (cffi:foreign-funcall-pointer init () :void))
        (let ((smoke-struct
                (cffi:mem-ref (cffi:foreign-symbol-pointer
                               (format nil "~A_Smoke" name))
                              :pointer))
              (data (cffi:foreign-alloc `(:struct ,(csymb '(qt SmokeData))))))
          (setf (svref (qtvar *module-table*) idx) smoke-struct)
          (setf (svref (qtvar *module-data-table*) idx) data)
          (qtcall sw_smoke smoke-struct
                  data
                  (cffi:get-callback (csymb '(qt deletion-callback)))
                  (cffi:get-callback (csymb '(qt method-invocation-callback)))))
        (incf (qtvar *n-modules*))
        idx))))

(defun patch-qt ()
  (swap-func '(qt load-libcommonqt) 'load-libcommonqt)
  (swap-func '(qt make-qapplication) 'make-qapplication)
  (swap-func '(qt ensure-smoke) 'ensure-smoke))

(defun unpatch-qt ()
  (restore-func '(qt load-libcommonqt))
  (restore-func '(qt make-qapplication))
  (restore-func '(qt ensure-smoke)))

(defun setup-paths ()
  (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
  #+windows (pushnew-path *standalone-libs-dir* "PATH")
  #+darwin (pushnew-path *standalone-libs-dir* "DYLD_LIBRARY_PATH")
  #+unix (pushnew-path *standalone-libs-dir* "LD_LIBRARY_PATH"))
(setup-paths)
