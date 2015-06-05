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
    #(5 2 136 84 160 200 159 24 138 215 17 65 174 155 228 189 29 132 236 26 217 185
      128 248 158 169 224 118 94 191 207 67 53 225 31 147 200 205 128 70 163 34 42
      207 245 230 167 118 228 243 6 31 145 89 167 40 172 137 252 202 49 65 118 201)))

(defmethod origin ((system (eql (asdf:find-system :smokegen))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "git://anongit.kde.org/smokegen"
        (format NIL "http://download.kde.org/stable/~a/src/smokegen-~:*~a.tar.xz" version))))

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
  (loop for dir in '(#+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokegen/")
        when (smokegen-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "install" "bin")))))))
