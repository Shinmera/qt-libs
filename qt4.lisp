#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :qt4
  :class build-system
  :pversion "qt-libs1.1.3")

(defmethod checksum ((system (eql (asdf:find-system :qt4))) &key type)
  (when (equal (version system) "qt-libs1.1.3")
    (case type
      (:sources
       #(65 10 12 211 225 28 227 128 246 208 73 44 104 62 132 196 218 188 152 81 226
         210 173 170 246 74 233 138 147 75 219 167 227 151 93 118 223 211 196 142 24
         86 171 124 82 222 74 58 137 250 122 165 52 5 24 248 250 243 231 224 28 250
         212 165))
      (:compiled
       #+(and linux x86-64)
       #(67 227 107 152 130 148 178 102 205 207 202 39 230 29 120 124 60 115 106 55 17
         25 125 229 126 56 149 36 210 54 230 170 195 7 35 65 33 169 145 126 238 238
         245 103 208 207 157 19 96 19 251 249 78 101 253 14 150 200 212 209 69 240 254
         241)
       #+(and darwin x86-64)
       #(135 193 243 48 210 89 51 101 105 32 86 202 100 163 117 115 55 26 152 149 40
         207 180 229 10 54 119 202 55 94 2 13 201 110 32 127 49 12 22 147 73 115 92
         143 89 26 180 12 237 97 16 14 233 137 54 43 159 69 71 221 209 255 81 178)
       #+(and windows x86)
       #(178 186 178 210 74 1 82 103 16 148 30 10 92 49 147 59 165 212 99 44 60 38 101
         157 188 100 19 8 4 7 82 54 94 70 111 14 128 206 95 72 174 176 20 250 104 9
         218 184 7 139 97 56 206 66 189 205 65 112 141 55 170 232 0 78)
       #+(and windows x86-64)
       #(229 159 204 93 189 243 232 122 106 130 155 254 13 181 99 254 100 110 133 34
         96 208 76 113 72 55 215 150 212 32 73 109 240 199 180 173 155 120 53 163 190
         17 54 227 63 77 137 112 144 214 67 76 158 210 210 199 65 142 233 5 221 42 76
         72)))))

(defmethod asdf:perform ((op download-op) (c (eql (asdf:find-system :qt4))))
  ;; There's really no point in downloading the sources for qt4
  (unless (eql (source-type op) :sources) 
    (call-next-method)))

(defmethod asdf:perform ((op generate-op) (c (eql (asdf:find-system :qt4))))
  NIL)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :qt4))))
  (test-prerequisite "Qt4.8" "qmake-qt4" "qmake"))

(defun qt4-on-path-p (path)
  (loop for file in (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4" :defaults path)
                          (make-pathname :name "QtCore" :defaults path))
        thereis (directory file)))

(defun find-qt-lib-directory ()
  (restart-case
      (or
       (loop for dir in '(#+windows #p"C:/Qt/4.8.7/bin/"
                          #+linux #p"/usr/lib/"
                          #+linux #p"/usr/local/lib/"
                          #+linux #p"/usr/lib64/qt48/"
                          #+linux #p"/usr/lib/*/"
                          #+osx-ports #p"/opt/local/lib/"
                          #+osx-ports #p"/opt/local/libexec/qt4/lib/"
                          #+osx-brew #p"/usr/local/Cellar/qt/4.8.7/lib/*.framework/"
                          #+osx-fink #p"/sw/lib/qt4-mac/lib/*.framework/")
             when (qt4-on-path-p dir)
             return dir)
       (error "Could not find Qt library directory!"))
    (specify-path (path)
      :report "Manually specify a path to use. If necessary, make the path wild to point to all possible library binaries."
      :interactive read-path
      path)))

(defun find-qt-plugins-directory ()
  (restart-case
      (or
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
             return (first resolved))
       (error "Could not find Qt plugins directory!"))
    (specify-path (path)
      :report "Manually specify a path to use. Make sure to make the path wild so that it points to all plugins subdirectories."
      :interactive read-path
      path)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :qt4))))
  (values (or (unless (eql (source-type op) :compiled)
                ;; Don't search when we're downloading anyway
                (ignore-errors (append (directory (find-qt-lib-directory))
                                       (directory (find-qt-plugins-directory)))))
              (append (call-next-method)
                      (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4"
                                                 :defaults (relative-dir "install" "lib"))))) T))

(defmethod shared-library-files ((system (eql (asdf:find-system :qt4))))
  (append
   (make-shared-library-files
    '("Qt3Support" "QtCLucene" "QtCore" "QtDBus" "QtDeclarative" "QtDesigner"
      "QtDesignerComponents" "QtGui" "QtHelp" "QtMultimedia" "QtNetwork"
      "QtOpenGL" "QtScript" "QtScriptTools" "QtSql" "QtSvg" "QtTest" "QtUiTools"
      "QtXml" "QtXmlPatterns" "QtWebKit" "phonon")
    (or (ignore-errors (find-qt-lib-directory))
        (relative-dir (first (asdf:output-files 'install-op system)) "lib"))
    :key #+windows (lambda (path) (make-pathname :name (format NIL "~a4" (pathname-name path)) :defaults path))
         #-windows #'identity)
   ;; These are additional libraries that are apparently provided by ports.
   #+osx-ports
   (make-shared-library-files
    '("z" "png" "ssl" "crypto" "dbus-1.3")
    #p"/opt/local/lib/")
   ;; Additional libraries that are stored in the Qt plugins folder.
   ;; The fun never ends. OH DEAR.
   (list (or (ignore-errors (find-qt-plugins-directory))
             (relative-dir (first (asdf:output-files 'install-op system)) "lib" "plugins")))))
