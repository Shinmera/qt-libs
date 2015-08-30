#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :libcommonqt
  :class build-system
  :pversion "qt-libs1.1.2"
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
  (when (equal (version system) "qt-libs1.1.2")
    (case type
      (:sources
       #(197 58 14 191 33 128 60 38 74 235 197 108 18 213 135 214 135 32 119
         205 7 220 115 35 52 58 121 186 176 94 109 214 207 52 234 225 143 38
         142 222 158 116 60 16 164 207 96 8 102 253 106 34 120 95 144 13 116
         156 179 173 99 117 43 198))
      (:compiled
       #+(and linux x86-64)
       #(105 78 163 136 66 52 35 63 247 113 88 240 178 72 105 10 90 152 63 37
         72 200 92 149 192 140 139 41 234 205 8 100 141 65 44 131 11 12 202 51
         15 177 22 113 209 246 230 126 220 155 229 225 148 254 133 237 196 165
         138 61 148 107 134 29)
       #+(and darwin x86-64)
       #(62 226 103 200 249 239 67 75 158 157 96 20 77 50 188 39 111 13 126 4
         245 68 104 80 143 193 120 67 51 62 94 184 54 130 92 31 126 101 109 102
         87 219 73 35 105 1 185 183 198 53 176 122 186 230 228 104 81 228 223
         195 2 130 153 177)
       #+(and windows x86)
       #(221 108 115 157 43 202 172 183 197 58 213 188 76 44 197 105 110 70 83
         191 46 183 152 203 12 109 107 85 224 98 220 219 244 85 175 123 102 191
         4 93 27 159 238 55 216 102 44 146 83 190 123 227 240 205 79 218 158
         164 204 45 112 14 144 196)
       #+(and windows x86-64)
       #(127 67 19 2 194 72 231 175 112 43 176 27 48 26 181 177 242 234 117 21
         106 94 95 58 198 113 41 167 41 192 20 103 159 55 148 202 103 17 60 119
         73 226 151 230 159 94 62 167 0 100 93 167 174 101 4 91 217 211 67 68
         250 151 98 155)))))

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
