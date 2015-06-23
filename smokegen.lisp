#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class cmake-build-system
  :version "4.14.3"
  :depends-on (:qt-build-prerequisites))

(defmethod checksum ((system (eql (asdf:find-system :smokegen))))
  (when (equal (asdf:component-version system) "4.14.3")
    #(64 75 137 70 243 85 119 42 252 132 226 155 84 3 129 3 171 74 159 98 162 60
      148 16 140 103 229 50 184 34 161 147 45 246 216 48 8 99 222 183 127 92 253
      105 161 33 107 208 251 111 171 171 255 94 2 89 129 161 232 176 21 154 3 53)))

(defmethod origin ((system (eql (asdf:find-system :smokegen))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "https://github.com/Shinmera/smokegen.git"
        (format NIL "https://github.com/Shinmera/smokegen/archive/v~a.tar.gz" version))))

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
