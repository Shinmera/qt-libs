#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class cmake-build-system
  :version "4.14.3"
  :depends-on (:qt-build-prerequisites
               :smokegen))

(defmethod checksum ((system (eql (asdf:find-system :smokeqt))))
  (when (equal (asdf:component-version system) "4.14.3")
    #(224 180 103 0 232 206 67 238 107 227 209 116 203 47 228 220 251 157 102 33 10
      145 38 49 155 172 201 51 121 201 174 198 98 5 111 127 7 108 136 197 90 249 51
      101 56 36 248 236 101 217 125 125 100 111 129 241 96 16 201 181 210 71 6 158)))

(defmethod origin ((system (eql (asdf:find-system :smokeqt))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "https://github.com/Shinmera/smokeqt.git"
        (format NIL "https://github.com/Shinmera/smokeqt/archive/v~a.tar.gz" version))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokeqt))))
  (let ((smoke-dir (first (asdf:output-files 'install-op (asdf:find-system :smokegen)))))
    (format NIL "-DCMAKE_BUILD_TYPE=Release ~
                 -DCMAKE_INSTALL_PREFIX=~s ~
                 -DWITH_Qwt5=OFF ~
                 -DSmoke_DIR=~s ~
                 -Wno-dev"
            (uiop:native-namestring (first (asdf:output-files 'install-op system)))
            (uiop:native-namestring (relative-dir smoke-dir "share" "smoke" "cmake")))))

(defmethod asdf:perform :around ((op generate-op) (system (eql (asdf:find-system :smokeqt))))
  (let* ((smoke-dir (first (asdf:output-files 'install-op (asdf:find-system :smokegen))))
         (*ld-library-path* (list* (uiop:native-namestring (relative-dir smoke-dir "lib"))
                                   (uiop:native-namestring (relative-dir smoke-dir "lib" "smokeqt"))
                                   *ld-library-path*)))
    (call-next-method)))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (list* (shared-library-file :name "libsmokeqtcore" :defaults (relative-dir "generate" "bin"))
         (call-next-method)))

(defun smokeqt-on-path-p (path)
  (uiop:file-exists-p
   (shared-library-file :name "libsmokeqtcore" :defaults (relative-dir path "lib"))))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokeqt))))
  (loop for dir in '(#+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokeqt/")
        when (smokeqt-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name "libsmokeqtcore" :defaults (relative-dir "install" "lib")))))))

