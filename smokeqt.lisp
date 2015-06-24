#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class cmake-build-system
  :pversion "qt-libs1.1.0"
  :depends-on (:qt-build-prerequisites
               :smokegen))

(defmethod checksum ((system (eql (asdf:find-system :smokeqt))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(244 217 211 112 254 21 112 251 160 77 229 246 173 212 58 20 82 184 248 51 110
         132 232 164 90 47 193 76 154 115 127 116 211 102 156 66 131 3 1 217 40 8 24
         78 40 99 103 67 63 123 216 194 1 111 95 116 69 53 26 211 101 42 220 72))
      (:compiled
       #(60 72 139 55 133 111 84 7 252 198 150 215 29 68 216 96 216 75 29 36 40 206 12
         175 197 6 191 203 140 102 18 172 184 213 34 233 135 7 81 210 217 179 62 134
         157 48 216 197 226 31 20 124 85 224 248 241 38 235 153 181 10 47 167 23)))))

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

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokeqt))))
  (list* (shared-library-file :name "smokeqtcore" :defaults (relative-dir "generate" "qtcore"))
         (call-next-method)))

(defun smokeqt-on-path-p (path)
  (uiop:file-exists-p
   (shared-library-file :name "smokeqtcore" :defaults (relative-dir path "lib"))))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokeqt))))
  (loop for dir in '(#+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+(and x86-64 windows) #p"C:/Program Files/smokeqt/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokeqt/")
        when (smokeqt-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name "smokeqtcore" :defaults (relative-dir "install" "lib")))))))

