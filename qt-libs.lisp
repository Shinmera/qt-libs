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
   #:setup-paths
   #:foreign-library-component
   #:foreign-library-system))
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

(defun %ensure-lib-loaded (file)
  (let ((file (etypecase file
                (pathname file)
                (string (installed-library-file file))))
        (name (intern (string-upcase (pathname-name file))))
        #+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    (cffi::register-foreign-library
     name `((T ,file))
     :search-path (to-directory file))
    (unless (cffi:foreign-library-loaded-p name)
      (cffi:load-foreign-library name))))

(defun ensure-lib-loaded (file)
  (cond ((pathnamep file)
         (%ensure-lib-loaded file))
        ((starts-with "smoke" file)
         (asdf:load-system (subseq file 5) :verbose NIL))
        (T
         (asdf:load-system file :verbose NIL))))

(defun setup-paths ()
  (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
  #+windows (pushnew-path *standalone-libs-dir* "PATH")
  #+darwin (pushnew-path *standalone-libs-dir* "DYLD_LIBRARY_PATH")
  #+unix (pushnew-path *standalone-libs-dir* "LD_LIBRARY_PATH"))
(setup-paths)

(defclass foreign-library-component (asdf:file-component)
  ()
  (:default-initargs :type #+windows "dll" #+darwin "dylib" #+linux "so"
                           #-(or windows darwin linux) NIL))

(defmethod asdf:perform ((op asdf:compile-op) (c foreign-library-component))
  (%ensure-lib-loaded (asdf:component-name c)))

(defmethod asdf:perform ((op asdf:load-op) (c foreign-library-component))
  (%ensure-lib-loaded (asdf:component-name c)))

(defclass foreign-library-system (asdf:system)
  ((library-files :accessor library-files :initarg :library-files :initform NIL)
   (smoke-module :accessor smoke-module :initarg :module :initform NIL)))

(defmacro f (pkg name &rest args)
  `(funcall (find-symbol ,(string name) ,(string pkg)) ,@args))

(defmethod asdf:perform :after ((op asdf:compile-op) (c foreign-library-system))
  (when (smoke-module c)
    (f qt initialize-smoke (smoke-module c))))

(defmethod asdf:perform :after ((op asdf:load-op) (c foreign-library-system))
  (when (smoke-module c)
    (f qt initialize-smoke (smoke-module c))))

(defun compile-foreign-library-system (name &key module depends-on library-files)
  `(asdf:defsystem ,(make-symbol (string-upcase name))
     :defsystem-depends-on (:qt-libs)
     :class "qt-libs:foreign-library-system"
     :version "1.0.0"
     :license "Artistic"
     :author "Nicolas Hafner <shinmera@tymoon.eu>"
     :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
     :description ,(format NIL "Loads the ~a foreign library." name)
     ,@(when module `(:module ,(string-upcase module)))
     :serial T
     :components ,(loop for file in library-files
                        collect `("qt-libs:foreign-library-component" ,file))
     :depends-on (:qt+libs ,@depends-on)))

(defun write-foreign-library-system (name &key module depends-on library-files path)
  (let ((path (or path (asdf:system-relative-pathname :qt-libs (format NIL "systems/~(~a~).asd" name)))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user))
            (*print-case* :downcase)
            (*print-pretty* T))
        (destructuring-bind (def name &rest kargs)
            (compile-foreign-library-system name :module module :depends-on depends-on :library-files library-files)
          (format stream "~
\(~s ~s~{
  ~s ~s~})"
                  def name kargs))))
    path))

;; Manually gathered from ldd/otool/depwalker information about the libraries
(defun generate-foreign-library-systems ()
  (macrolet ((g (&body defs)
               `(list
                 ,@(loop for def in defs
                         collect (destructuring-bind (name &key (module-p T) depends-on library-files) def
                                   `(write-foreign-library-system
                                     ',name :depends-on ',depends-on
                                            :library-files ',library-files
                                            :module ,(when module-p `',name)))))))
    (g (smokebase
        :library-files ("smokebase")
        :module-p NIL)
       (commonqt
        :depends-on (:smokebase)
        :library-files ("QtCore" "QtGui" "commonqt")
        :module-p NIL)
       (phonon
        :depends-on (:qtcore :qtgui
                     (:feature (:or :linux :darwin) :qtdbus)
                     (:feature (:or :linux :darwin) :qtxml))
        :library-files ("phonon" "smokephonon"))
       (qimageblitz
        :depends-on (:qtcore :qtgui)
        :library-files ("qimageblitz" "smokeqimageblitz"))
       (qsci
        :depends-on (:qtcore :qtgui)
        :library-files ("qscintilla2" "smokeqsci"))
       (qt3support
        :depends-on (:qtcore :qtgui :qtxml :qtnetwork :qtsql)
        :library-files ("Qt3Support" "smokeqt3support"))
       (qtcore
        :depends-on (:commonqt)
        :library-files ("QtCore" "smokeqtcore"))
       (qtdbus
        :depends-on (:qtcore :qtxml)
        :library-files ("QtDBus" "smokeqtdbus"))
       (qtdeclarative
        :depends-on (:qtcore :qtgui :qtnetwork :qtscript :qtsql :qtxmlpatterns)
        :library-files ("QtDeclarative" "smokeqtdeclarative"))
       (qtgui
        :depends-on (:qtcore)
        :library-files ("QtGui" "smokeqtgui"))
       (qthelp
        :depends-on (:qtcore :qtgui :qtnetwork :qtsql)
        :library-files ("QtCLucene" "QtHelp" "smokeqthelp"))
       (qtnetwork
        :depends-on (:qtcore)
        :library-files ("QtNetwork" "smokeqtnetwork"))
       (qtopengl
        :depends-on (:qtcore :qtgui)
        :library-files ("QtOpenGL" "smokeqtopengl"))
       (qtscript
        :depends-on (:qtcore)
        :library-files ("QtScript" "smokeqtscript"))
       (qtsql
        :depends-on (:qtcore :qtgui)
        :library-files ("QtSql" "smokeqtsql"))
       (qtsvg
        :depends-on (:qtcore :qtgui)
        :library-files ("QtSvg" "smokeqtsvg"))
       (qttest
        :depends-on (:qtcore :qtgui)
        :library-files ("QtTest" "smokeqttest"))
       (qtuitools
        :depends-on (:qtcore :qtgui)
        :library-files ("smokeqtuitools"))
       (qtwebkit
        :depends-on (:qtcore :qtgui :qtnetwork)
        :library-files ("QtWebKit" "smokeqtwebkit"))
       (qwt
        :depends-on (:qtcore :qtgui
                     (:feature :windows :qtsvg))
        :library-files ("qwt" "smokeqwt"))
       (qtxmlpatterns
        :depends-on (:qtcore :qtnetwork)
        :library-files ("QtXmlPatterns" "smokeqtxmlpatterns"))
       (qtxml
        :depends-on (:qtcore)
        :library-files ("QtXml" "smokeqtxml")))))
