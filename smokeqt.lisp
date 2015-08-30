#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class cmake-build-system
  :pversion "qt-libs1.1.2"
  :depends-on (#-windows :qt-build-prerequisites
               :smokegen))

(defmethod checksum ((system (eql (asdf:find-system :smokeqt))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(108 90 185 171 223 253 193 216 61 232 182 207 199 114 170 209 233 72
         181 51 156 9 84 32 248 251 47 184 26 218 123 190 97 56 73 232 115 194
         142 185 249 13 168 34 141 192 245 152 228 129 217 241 26 28 170 44 82
         108 172 191 131 224 31 89))
      (:compiled
       #+(and linux x86-64)
       #(92 31 150 142 43 211 228 102 207 116 111 195 101 247 43 83 97 237 155
         17 100 0 128 130 111 53 233 142 37 3 171 105 167 20 242 23 115 126 227
         111 41 204 46 27 69 59 103 122 138 145 7 16 115 209 184 213 46 82 43
         62 95 113 225 207)
       #+(and darwin x86-64)
       #(53 116 185 18 34 92 81 139 116 227 52 124 175 249 186 18 155 158 203
         113 59 60 244 165 215 144 95 152 143 128 180 184 165 165 168 220 234
         179 175 233 252 163 254 230 54 77 200 44 47 94 180 230 55 33 232 183
         40 88 186 82 224 19 0 218)
       #+(and windows x86)
       #(238 115 65 199 147 115 205 3 101 241 1 116 209 159 22 47 70 162 237 74
         114 101 129 219 22 63 40 237 66 16 56 158 222 223 65 145 156 23 4 144
         41 48 121 65 243 134 179 122 73 114 199 121 179 85 51 194 248 79 24 93
         14 162 113 235)
       #+(and windows x86-64)
       #(72 25 212 135 209 45 176 38 177 24 28 75 103 50 198 180 182 243 209
         144 44 229 245 40 159 4 150 37 41 249 104 224 154 81 145 54 163 129
         221 207 231 253 173 141 184 180 217 112 43 160 77 242 0 124 114 183
         164 232 30 212 51 129 71 140)))))

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
   (second (asdf:output-files 'install-op system))))
