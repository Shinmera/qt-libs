#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :libcommonqt
  :class build-system
  :pversion "qt-libs1.1.0"
  :depends-on (#-windows :qt-build-prerequisites
               :qt4
               :smokegen
               :smokeqt))

(defun fix-commonqt-pro-file (file &rest basepaths)
  (let ((contents (uiop:read-file-string file)))
    (unless (search "Qtools Fix" contents)
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :error)
        (format stream "~&# Qtools fix~%")
        (dolist (base basepaths)
          (format stream "~&LIBS += -L~s~
                          ~&INCLUDEPATH += ~s"
                  (uiop:native-namestring (relative-dir base "lib/"))
                  (uiop:native-namestring (relative-dir base "include/"))))
        (format stream "~&~a" contents))
      file)))

(defmethod checksum ((system (eql (asdf:find-system :libcommonqt))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(210 129 55 59 150 119 82 240 111 58 140 131 183 25 2 169 114 237 32 96 207
         154 215 185 105 96 159 30 76 249 211 34 113 72 121 244 51 130 23 243 28 50
         206 239 200 229 216 181 92 149 226 85 224 62 177 9 43 69 81 61 159 87 208 195))
      (:compiled
       #+(and linux x86-64)
       #(191 101 105 12 81 86 41 121 10 34 196 243 229 31 255 222 79 138 140 84 115
         143 58 79 184 126 125 80 13 41 25 192 74 197 253 40 63 199 116 186 1 93 121
         96 67 87 103 218 223 148 246 21 224 254 120 149 96 17 245 55 147 49 151 145)
       #+(and darwin x86-64)
       #(187 4 180 141 254 54 62 11 249 67 222 132 254 115 147 65 67 224 213 62 238 32
         29 244 115 44 204 127 127 149 121 164 220 78 68 68 190 197 32 238 215 92 134
         62 168 199 30 112 159 237 77 119 200 250 213 228 191 46 39 121 144 75 192 213)
       #+(and windows x86-64)
       #(127 67 19 2 194 72 231 175 112 43 176 27 48 26 181 177 242 234 117 21 106 94
         95 58 198 113 41 167 41 192 20 103 159 55 148 202 103 17 60 119 73 226 151
         230 159 94 62 167 0 100 93 167 174 101 4 91 217 211 67 68 250 151 98 155)
       #+(and windows x86)
       #(138 203 241 237 4 200 239 36 119 128 60 78 197 228 3 70 25 176 113 215 47 38
         85 169 64 12 71 189 146 155 193 207 11 175 238 145 4 140 191 233 239 39 112
         155 181 104 221 174 94 57 84 77 158 85 25 170 129 236 11 183 57 255 122 138)))))

(defmethod asdf:input-files ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (list (make-pathname :name "commonqt" :type "pro" :defaults (first (asdf:output-files 'download-op system)))))

(defmethod asdf:perform ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (let ((project-file (first (asdf:input-files op system))))
    (fix-commonqt-pro-file project-file
                           (first (asdf:output-files 'install-op (asdf:find-system :smokeqt)))
                           (first (asdf:output-files 'install-op (asdf:find-system :smokegen))))
    (let ((makefile (make-pathname :name "Makefile" :type NIL :defaults project-file)))
      (run-here "`command -v qmake-qt4 || command -v qmake` ~a~s -o ~s"
                #+darwin "-spec macx-g++ " #-darwin ""
                (uiop:native-namestring project-file)
                (uiop:native-namestring makefile))
      (run-here "make -C ~s"
                (uiop:native-namestring (uiop:pathname-directory-pathname makefile))))))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (values (list (shared-library-file :name "commonqt" :defaults (first (asdf:output-files 'download-op system))))
          T))

(defmethod asdf:perform ((op install-op) (system (eql (asdf:find-system :libcommonqt))))
  T)

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :libcommonqt))))
  (asdf:output-files 'generate-op system))

(defmethod shared-library-files ((system (eql (asdf:find-system :libcommonqt))))
  (mapcar #'uiop:resolve-symlinks (asdf:output-files 'install-op system)))
