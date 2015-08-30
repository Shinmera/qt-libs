#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class cmake-build-system
  :pversion "qt-libs1.1.2"
  :depends-on (#-windows :qt-build-prerequisites))

(defmethod checksum ((system (eql (asdf:find-system :smokegen))) &key type)
  (when (equal (version system) "qt-libs1.1.2")
    (case type
      (:sources
       #(201 78 139 247 130 77 121 27 211 169 174 234 41 17 46 77 196 106 120
         120 118 255 190 98 200 134 100 65 191 207 165 8 20 159 125 92 33 247
         19 87 236 18 100 189 227 165 94 100 11 244 112 106 43 168 168 77 52 85
         81 54 30 124 42 49))
      (:compiled
       #+(and linux x86-64)
       #(46 187 27 81 216 24 244 213 97 111 136 3 224 63 226 130 128 66 254 141
         89 170 101 95 104 213 179 95 102 131 56 94 179 44 187 192 137 85 233
         64 106 237 81 142 134 122 96 223 50 130 68 215 61 11 70 200 177 10 28
         27 99 97 32 157)
       #+(and darwin x86-64)
       #(222 30 73 137 103 37 44 117 186 29 102 96 60 77 148 166 189 46 105 240
         224 119 11 9 237 226 188 74 153 254 205 150 105 42 6 113 106 120 53 42
         22 100 138 68 184 13 3 166 78 42 218 19 77 248 118 35 6 10 82 149 120
         65 135 165)
       #+(and windows x86)
       #(163 6 41 5 248 153 147 113 116 12 19 215 68 36 120 109 235 191 24 101
         8 29 227 1 70 173 220 201 58 21 205 16 60 209 87 30 145 175 214 54 30
         66 158 87 35 253 15 9 39 180 117 169 7 145 173 229 75 116 105 250 62
         10 215 21)
       #+(and windows x86-64)
       #(202 30 3 12 254 168 98 197 131 212 97 104 22 102 48 61 50 172 81 5 100
         62 138 167 81 38 162 240 105 72 80 65 232 185 67 24 174 144 229 218 82
         81 6 189 107 3 148 167 36 196 101 193 6 127 23 20 142 80 175 26 24 107
         26 60)))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokegen))))
  (format NIL "-DCMAKE_BUILD_TYPE=Release ~
               -DCMAKE_INSTALL_PREFIX=~s"
          (uiop:native-namestring (first (asdf:output-files 'install-op system)))))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (append (call-next-method)
          (list (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "generate" "bin")))))

(defun smokegen-on-path-p (path)
  (uiop:file-exists-p
   (shared-library-file :name "smokebase" :defaults path)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokegen))))
  (loop for dir in '(#+unix #p"/usr/lib/"
                     #+unix #p"/usr/local/lib/"
                     #+(and x86-64 unix) #p"/usr/lib64/"
                     #+(and x86-64 windows) #p"C:/Program Files/smokegenerator/bin/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokegenerator/bin/")
        when (smokegen-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (shared-library-file :name "smokebase" :defaults (relative-dir "install" "lib")))))))

(defmethod shared-library-files ((system (eql (asdf:find-system :smokegen))))
  (make-shared-library-files
   '("smokebase"
     #+windows "msvcp100"
     #+windows "msvcp110"
     #+windows "msvcr100"
     #+windows "msvcr110")
   (second (asdf:output-files 'install-op system))))
