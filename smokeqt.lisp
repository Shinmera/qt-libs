#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class cmake-build-system
  :pversion "qt-libs1.1.4"
  :depends-on (#-windows :qt-build-prerequisites
               :smokegen))

(defmethod checksum ((system (eql (asdf:find-system :smokeqt))) &key type)
  (when (equal (version system) "qt-libs1.1.4")
    (case type
      (:sources
       #(45 221 238 135 239 211 155 139 22 86 18 230 167 214 221 255 176 143 125 130
         67 67 182 192 245 224 218 156 97 170 94 0 191 119 181 232 16 254 73 20 104
         146 208 99 111 204 108 106 97 66 159 127 55 63 135 67 102 148 150 249 4 24
         142 55))
      (:compiled
       #+(and linux x86-64)
       #()
       #+(and darwin x86-64)
       #()
       #+(and windows x86)
       #()
       #+(and windows x86-64)
       #()))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokeqt))))
  (let ((smoke-dir (first (asdf:output-files 'install-op (asdf:find-system :smokegen)))))
    (format NIL "-DCMAKE_BUILD_TYPE=Release ~
                 -DCMAKE_INSTALL_PREFIX=~s ~
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
  (append (call-next-method)
          (list (shared-library-file :name "smokeqtcore" :defaults (relative-dir "generate" "qtcore")))))

(defun smokeqt-on-path-p (path)
  (uiop:file-exists-p
   (shared-library-file :name "smokeqtcore" :defaults path)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokeqt))))
  (loop for dir in '(#+unix #p"/usr/lib/"
                     #+unix #p"/usr/local/lib/"
                     #+(and x86-64 unix) #p"/usr/lib64/"
                     #+(and x86-64 windows) #p"C:/Program Files/smokeqt/bin/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokeqt/bin/")
        when (smokeqt-on-path-p dir)
        return (values (list dir dir) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name "smokeqtcore" :defaults (relative-dir "install" "lib")))))))

(defmethod shared-library-files ((system (eql (asdf:find-system :smokeqt))))
  (make-shared-library-files
   '("smokephonon" "smokeqimageblitz" "smokeqsci" "smokeqt3support"
     "smokeqtcore" "smokeqtdbus" "smokeqtdeclarative" "smokeqtgui" "smokeqthelp"
     "smokeqtmultimedia" "smokeqtnetwork" "smokeqtopengl" "smokeqtscript"
     "smokeqtsql" "smokeqtsvg" "smokeqttest" "smokeqtuitools" "smokeqtwebkit"
     "smokeqwt" "smokeqtxml" "smokeqtxmlpatterns")
   (second (asdf:output-files 'install-op system))))
