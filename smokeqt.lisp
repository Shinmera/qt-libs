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
    #(144 25 216 166 92 186 123 5 64 31 58 96 125 76 139 105 59 209 79 49 205 117
      153 86 68 94 3 5 120 124 145 196 195 106 58 227 57 68 124 197 115 88 87 128
      102 191 39 5 202 7 52 20 21 179 31 208 30 165 1 141 111 190 108 113)))

(defmethod origin ((system (eql (asdf:find-system :smokeqt))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "git://anongit.kde.org/smokeqt"
        (format NIL "http://download.kde.org/stable/~a/src/smokeqt-~:*~a.tar.xz" version))))

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
