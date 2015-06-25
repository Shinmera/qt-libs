#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class cmake-build-system
  :pversion "qt-libs1.1.0"
  :depends-on (#-windows :qt-build-prerequisites
               :smokegen))

(defmethod checksum ((system (eql (asdf:find-system :smokeqt))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(244 217 211 112 254 21 112 251 160 77 229 246 173 212 58 20 82 184 248 51 110
         132 232 164 90 47 193 76 154 115 127 116 211 102 156 66 131 3 1 217 40 8 24
         78 40 99 103 67 63 123 216 194 1 111 95 116 69 53 26 211 101 42 220 72))
      (:compiled
       #+(and linux x86-64)
       #(60 72 139 55 133 111 84 7 252 198 150 215 29 68 216 96 216 75 29 36 40 206 12
         175 197 6 191 203 140 102 18 172 184 213 34 233 135 7 81 210 217 179 62 134
         157 48 216 197 226 31 20 124 85 224 248 241 38 235 153 181 10 47 167 23)
       #+(and darwin x86-64)
       #(211 92 52 128 97 7 146 193 93 132 5 78 194 18 235 170 69 155 197 46 200 210
         225 72 219 53 70 166 190 95 211 56 1 157 75 220 66 110 30 250 96 69 65 179
         254 197 116 104 145 104 133 73 88 113 57 3 155 101 214 138 199 174 141 35)
       #+(and windows x86-64)
       #(151 20 4 131 53 197 212 229 90 180 191 25 211 225 8 92 254 220 58 236 6 7 90
         120 212 119 106 159 61 149 180 246 14 190 140 143 121 5 38 151 193 95 4 254
         126 201 236 0 77 68 144 164 150 112 75 209 199 26 42 78 37 143 34 83)
       #+(and windows x86)
       #(203 174 236 177 56 0 181 41 254 61 20 203 26 68 29 20 108 137 103 168 35 134
         241 237 15 234 52 3 130 219 235 229 70 176 51 204 232 132 72 201 65 232 2 209
         71 52 139 225 93 4 54 200 78 198 38 224 208 227 242 208 34 245 39 143)))))

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
   (shared-library-file :name "smokeqtcore" :defaults path)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokeqt))))
  (loop for dir in '(#+unix #p"/usr/lib/"
                     #+unix #p"/usr/local/lib/"
                     #+(and x86-64 unix) #p"/usr/lib64/"
                     #+(and x86-64 windows) #p"C:/Program Files/smokeqt/bin/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokeqt/bin/")
        when (smokeqt-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name "smokeqtcore" :defaults (relative-dir "install" "lib")))))))

(defmethod shared-library-files ((system (eql (asdf:find-system :smokeqt))))
  (make-shared-library-files
   '("smokephonon" "smokeqimageblitz" "smokeqsci" "smokeqt3support"
     "smokeqtcore" "smokeqtdbus" "smokeqtdeclarative" "smokeqtgui" "smokeqthelp"
     "smokeqtmultimedia" "smokeqtnetwork" "smokeqtopengl" "smokeqtscript"
     "smokeqtsql" "smokeqtsvg" "smokeqttest" "smokeqtuitools" "smokeqtwebkit"
     "smokeqtxml" "smokeqtxmlpatterns")
   (first (asdf:output-files 'install-op system))))
