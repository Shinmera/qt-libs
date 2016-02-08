#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :qt4
  :class build-system
  :pversion "qt-libs1.1.2")

(defmethod checksum ((system (eql (asdf:find-system :qt4))) &key type)
  (when (equal (version system) "qt-libs1.1.2")
    (case type
      (:sources
       #(233 19 145 25 252 101 76 146 128 129 125 179 139 126 143 191 55 196 20
         70 111 122 16 184 243 76 62 71 223 117 190 11 86 197 123 93 59 249 129
         158 31 144 174 195 109 201 3 100 3 84 18 3 203 169 189 57 42 28 119 96
         154 16 165 45))
      (:compiled
       #+(and linux x86-64)
       #(243 245 181 6 104 56 173 180 225 208 106 57 51 88 84 172 234 236 242
         87 206 93 93 233 39 124 12 25 223 224 128 250 181 63 169 69 36 3 144
         241 245 89 30 101 31 211 87 177 104 83 207 86 192 96 16 155 215 226
         217 246 123 236 163 178)
       #+(and darwin x86-64)
       #(4 85 75 131 60 7 100 155 49 90 224 209 55 244 113 11 236 92 85 9 94
         230 33 37 118 70 121 180 70 21 245 10 4 231 118 89 37 249 234 170 232
         253 180 30 145 212 225 228 38 198 161 140 108 82 167 45 254 207 93 242
         136 143 11 90)
       #+(and windows x86)
       #(248 235 77 0 153 134 192 93 116 163 84 22 175 204 40 133 164 253 10 0
         239 69 237 246 188 173 115 211 183 50 226 132 9 234 197 237 116 122
         151 142 41 225 45 163 167 242 22 138 195 209 101 152 111 56 171 21 217
         147 95 108 190 59 39 91)
       #+(and windows x86-64)
       #(56 111 7 220 145 45 194 149 78 162 31 171 26 204 139 51 12 28 45 146
         11 57 31 108 248 246 49 23 62 112 151 93 86 102 246 177 108 205 73 101
         29 133 110 181 39 147 224 78 101 27 180 56 13 23 158 26 159 182 176
         117 121 165 112 201)))))

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
             return resolved)
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
