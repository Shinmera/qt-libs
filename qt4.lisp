#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass qt4 (locally-available-library github-library checksummed-library)
  ()
  (:default-initargs :tag "qt-libs2.0.0"))

(defun qt4-on-path-p (path)
  (loop for file in (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4" :defaults path)
                          (make-pathname :name "QtCore" :defaults path))
        thereis (directory file)))

(defun find-qt-lib-directory ()
  (loop for dir in '(#+windows #p"C:/Qt/4.8.7/bin/"
                     #+linux #p"/usr/lib/"
                     #+linux #p"/usr/local/lib/"
                     #+linux #p"/usr/lib64/qt48/"
                     #+linux #p"/usr/lib64/"
                     #+linux #p"/usr/lib/*/"
                     #+osx-ports #p"/opt/local/lib/"
                     #+osx-ports #p"/opt/local/libexec/qt4/lib/"
                     #+osx-brew #p"/usr/local/Cellar/qt/4.8.7*/lib/*.framework/"
                     #+osx-fink #p"/sw/lib/qt4-mac/lib/*.framework/")
        when (qt4-on-path-p dir)
        return dir))

(defun find-qt-plugins-directory ()
  (loop for dir in '(#+windows #p"C:/Qt/4.8.7/plugins/"
                     #+linux #p"/usr/lib/qt4/plugins/"
                     #+linux #p"/usr/local/lib/qt4/plugins/"
                     #+linux #p"/usr/lib/*/qt4/plugins/"
                     #+osx-ports #p"/opt/local/share/qt4/plugins/"
                     #+osx-ports #p"/opt/local/libexec/qt4/share/plugins/"
                     #+osx-brew #p"/usr/local/Cellar/qt/4.8.7/plugins/"
                     #+osx-fink #p"/sw/lib/qt4-mac/plugins/")
        for resolved = (directory dir)
        when resolved
        return (first resolved)))

(defmethod find-local-files ((system qt4))
  (append
   (make-shared-library-files
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
      "phonon")
    (find-qt-lib-directory)
    :key #+windows (lambda (path) (make-pathname :name (format NIL "~a4" (pathname-name path)) :defaults path))
         #-windows #'identity)
   ;; These are additional libraries that are apparently provided by ports.
   #+osx-ports
   (make-shared-library-files
    '("z" "png" "ssl" "crypto" "dbus-1.3")
    #p"/opt/local/lib/")
   ;; Additional libraries that are stored in the Qt plugins folder.
   ;; The fun never ends. OH DEAR.
   (list (find-qt-plugins-directory))))
