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
       #+(and linux x86-64)
       #(60 72 139 55 133 111 84 7 252 198 150 215 29 68 216 96 216 75 29 36 40 206 12
         175 197 6 191 203 140 102 18 172 184 213 34 233 135 7 81 210 217 179 62 134
         157 48 216 197 226 31 20 124 85 224 248 241 38 235 153 181 10 47 167 23)
       #+(and darwin x86-64)
       #(211 92 52 128 97 7 146 193 93 132 5 78 194 18 235 170 69 155 197 46 200 210
         225 72 219 53 70 166 190 95 211 56 1 157 75 220 66 110 30 250 96 69 65 179
         254 197 116 104 145 104 133 73 88 113 57 3 155 101 214 138 199 174 141 35)
       #+(and windows x86-64)
       #(132 216 113 205 66 35 72 94 201 12 203 202 102 246 38 183 204 184 82 154 216
         126 244 25 146 148 13 198 55 72 27 109 180 173 158 21 29 11 37 181 245 80 57
         149 123 27 208 100 45 226 126 241 237 35 5 162 33 155 207 222 16 243 184 209)
       #+(and windows x86)
       #(184 245 12 207 148 229 121 231 138 216 56 30 110 226 48 94 11 102 38 209 91
         57 26 18 177 46 216 215 111 71 146 102 24 3 55 81 56 88 224 27 132 39 245 228
         36 169 138 237 58 203 149 215 117 180 171 34 62 145 48 18 5 0 255 242)))))

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

