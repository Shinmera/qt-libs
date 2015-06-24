#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class cmake-build-system
  :pversion "qt-libs1.1.0"
  :depends-on (:qt-build-prerequisites))

(defmethod checksum ((system (eql (asdf:find-system :smokegen))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(41 54 7 141 196 229 30 13 214 153 125 233 202 119 253 150 153 49 254 242 67
         196 235 211 217 147 84 210 23 55 7 88 98 54 129 2 46 215 9 38 241 63 107 44
         171 30 59 153 42 153 233 88 46 191 83 122 12 152 253 45 249 7 228 108))
      (:compiled
       #(4 48 91 203 66 39 36 70 228 106 47 167 250 58 199 88 14 109 177 70 245 106
         129 156 125 205 203 118 77 39 108 48 47 2 188 72 198 255 53 201 129 19 241
         156 187 204 113 79 16 213 120 199 86 209 180 228 15 72 41 47 137 239 228 101)))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokegen))))
  (format NIL "-DCMAKE_BUILD_TYPE=Release ~
               -DCMAKE_INSTALL_PREFIX=~s"
          (uiop:native-namestring (first (asdf:output-files 'install-op system)))))

(defun smokegen-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "smokegen" :defaults (relative-dir path "bin"))))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (list* (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "generate" "bin"))
         (call-next-method)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokegen))))
  (loop for dir in '(#+unix #p"/usr"
                     #+unix #p"/usr/local"
                     #+(and x86-64 windows) #p"C:/Program Files/smokegenerator/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokegenerator/")
        when (smokegen-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "install" "bin")))))))
