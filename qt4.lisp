#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :qt4
  :class build-system
  :pversion "qt-libs1.1.0")

(defmethod checksum ((system (eql (asdf:find-system :qt4))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:compiled
       #+(and darwin x86-64)
       #(73 127 134 127 117 93 171 94 24 126 237 182 244 75 135 37 52 145 162 29 218
         70 184 7 115 75 95 195 150 35 160 104 42 22 184 229 20 45 108 92 15 100 168
         74 202 10 4 180 89 199 128 193 69 236 71 168 62 251 48 12 139 81 94 58)
       #+(and windows x86-64)
       #(182 176 149 24 84 34 246 199 40 105 82 131 34 128 70 255 18 60 123 222 215 8
         217 63 142 15 192 134 139 149 56 81 60 154 99 102 190 212 110 111 141 55 250
         9 206 239 144 183 226 226 127 140 127 69 158 31 53 250 116 71 229 106 216 70)
       #+(and windows x86)
       #(215 80 58 140 199 236 63 55 20 214 169 72 179 211 216 138 39 238 140 49 22
         170 14 56 62 74 146 150 225 173 68 115 55 70 227 72 119 123 3 16 44 225 146
         156 221 113 36 170 13 240 159 72 108 90 122 152 197 192 243 45 5 103 210 136)))))

(defmethod asdf:perform ((op generate-op) (c (eql (asdf:find-system :qt4))))
  NIL)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :qt4))))
  (test-prerequisite "Qt4.8" "qmake-qt4" "qmake"))

(defun qt4-on-path-p (path)
  (loop for file in (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4" :defaults path)
                          (make-pathname :name "QtCore" :defaults path))
        thereis (directory file)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :qt4))))
  (loop for dir in '(#+windows #p"C:/Qt/4.8.7/bin/"
                     #+linux #p"/usr/lib/"
                     #+linux #p"/usr/local/lib/"
                     #+linux #.(uiop:wilden #p"/usr/lib/")
                     #+linux #p"/usr/lib64/qt48/"
                     #+darwin #p"/opt/local/lib/")
        for found = (qt4-on-path-p dir)
        when found
        return (values (if (wild-pathname-p dir)
                           found
                           (list dir)) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name #+unix "QtCore" #+windows "QtCore4"
                                                           :defaults (relative-dir "install" "lib")))))))

(defmethod shared-library-files ((system (eql (asdf:find-system :qt4))))
  (make-shared-library-files
   '("Qt3Support" "QtCLucene" "QtCore" "QtDBus" "QtDeclarative" "QtDesigner"
     "QtDesignerComponents" "QtGui" "QtHelp" "QtMultimedia" "QtNetwork"
     "QtOpenGL" "QtScript" "QtScriptTools" "QtSql" "QtSvg" "QtTest" "QtUiTools"
     "QtXml" "QtXmlPatterns" "phonon")
   (let ((dirs (asdf:output-files 'install-op system)))
     (or (second dirs) (first dirs)))
   :key #+windows (lambda (path)
                    (make-pathname :name (format NIL "~a4" (pathname-name path))
                                   :defaults path))
        #-windows #'identity))
