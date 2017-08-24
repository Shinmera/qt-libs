#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *generic-library-directories*
  '(#+unix #p"/usr/local/lib/"
    #+unix #p"/usr/local/lib/*/"
    #+linux #p"/usr/lib64/"
    #+linux #p"/usr/lib/x86_64-linux-gnu/"
    #+unix #p"/usr/lib/*/"
    #+unix #p"/usr/lib/"
    #+osx-ports #p"/opt/local/libexec/qt4/lib/"
    #+osx-ports #p"/opt/local/lib/"
    #+osx-brew #p"/usr/local/Cellar/*/*/lib/"))

(defvar *qt-module-list*
  '("Qt3Support"
    "QtCLucene"
    "QtCore"
    "QtDBus"
    "QtDeclarative"
    "QtDesigner"
    "QtDesignerComponents"
    "QtGui"
    "QtHelp"
    "QtMultimedia"
    "QtNetwork"
    "QtOpenGL"
    "QtScript"
    "QtScriptTools"
    "QtSql"
    "QtSvg"
    "QtTest"
    "QtUiTools"
    "QtXml"
    "QtXmlPatterns"
    "QtWebKit"
    "phonon"))

(defclass qt4 (locally-available-library github-library checksummed-library)
  ()
  (:default-initargs :tag "qt-libs2.0.1"))

(defun qt4-on-path-p (path)
  (loop for file in (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4" :defaults path)
                          (make-pathname :name "QtCore" :defaults path))
        thereis (directory file)))

(defun find-qt-lib-directory ()
  (loop for dir in (append '(#+windows #p"C:/Qt/4.8.7/bin/"
                             #+linux #p"/usr/local/Trolltech/Qt-4.8.7/lib/"
                             #+linux #p"/usr/lib64/qt48/"
                             #+osx-ports #p"/opt/local/libexec/qt4/lib/"
                             #+osx-brew #p"/usr/local/Cellar/qt/*/lib/*.framework/"
                             #+osx-fink #p"/sw/lib/qt4-mac/lib/*.framework/")
                           *generic-library-directories*)
        when (qt4-on-path-p dir)
        collect dir))

(defun find-qt-plugins-directory ()
  (loop for dir in '(#+windows #p"C:/Qt/4.8.7/plugins/"
                     #+linux #p"/usr/local/Trolltech/Qt-4.8.7/plugins/"
                     #+linux #p"/usr/local/lib/qt4/plugins/"
                     #+linux #p"/usr/lib/x86_64*/qt4/plugins/"
                     #+unix #p"/usr/local/lib/*/plugins/"
                     #+unix #p"/usr/lib/qt4/plugins/"
                     #+unix #p"/usr/lib/*/qt4/plugins/"
                     #+osx-ports #p"/opt/local/share/qt4/plugins/"
                     #+osx-ports #p"/opt/local/libexec/qt4/share/plugins/"
                     #+osx-brew #p"/usr/local/Cellar/qt/*/plugins/"
                     #+osx-fink #p"/sw/lib/qt4-mac/plugins/")
        for resolved = (directory dir)
        when resolved
        return resolved))

(defmethod find-local-files ((system qt4))
  (append
   (make-shared-library-files
    *qt-module-list*
    (append '(#+osx-ports #p"/opt/local/lib/")
            (find-qt-lib-directory))
    :key #+windows (lambda (path) (make-pathname :name (format NIL "~a4" (pathname-name path)) :defaults path))
         #+osx-brew (lambda (path) (make-pathname :name (subseq (pathname-name path) 3) :type :wild :defaults path))
         #-(or windows osx-brew) #'identity)
   (make-shared-library-files
    '("qscintilla2"
      "qimageblitz"
      "qwt-qt4"
      "qwt.5"
      "qwt5"
      "qwt")
    (append '(#+unix #p"/usr/local/qwt/lib/"
              #+unix #p"/usr/local/Trolltech/Qt-4.8.7/lib/"
              #+osx-brew #p"/usr/local/Cellar/qwt/*/lib/qwt.framework/"
              #+osx-brew #p"/usr/local/Cellar/qscintilla2/*/lib/")
            *generic-library-directories*))
   ;; These are additional libraries that are apparently provided by ports.
   #+osx-ports
   (make-shared-library-files
    '("z" "png16" "ssl" "crypto" "dbus-1.3")
    #p"/opt/local/lib/")
   ;; Additional libraries that are stored in the Qt plugins folder.
   ;; The fun never ends. OH DEAR.
   (find-qt-plugins-directory)))
