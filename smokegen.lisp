#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class cmake-build-system
  :pversion "qt-libs1.1.0"
  :depends-on (:qt-build-prerequisites))

(defmethod checksum ((system (eql (asdf:find-system :smokegen))) &key type)
  (when (equal (version system) "qt-libs1.1.0")
    (case type
      (:sources
       #(41 54 7 141 196 229 30 13 214 153 125 233 202 119 253 150 153 49 254 242 67
         196 235 211 217 147 84 210 23 55 7 88 98 54 129 2 46 215 9 38 241 63 107 44
         171 30 59 153 42 153 233 88 46 191 83 122 12 152 253 45 249 7 228 108))
      (:compiled
       #+(and linux x86-64)
       #(4 48 91 203 66 39 36 70 228 106 47 167 250 58 199 88 14 109 177 70 245 106
         129 156 125 205 203 118 77 39 108 48 47 2 188 72 198 255 53 201 129 19 241
         156 187 204 113 79 16 213 120 199 86 209 180 228 15 72 41 47 137 239 228 101)
       #+(and darwin x86-64)
       #(218 177 182 207 34 7 129 130 226 230 248 153 17 227 252 88 79 189 237 75 100
         235 70 43 123 189 153 122 141 208 68 67 76 195 67 210 99 5 13 6 166 233 194
         32 152 111 94 234 24 71 162 124 43 144 209 192 172 144 169 33 250 208 232 22)
       #+(and windows x86-64)
       #(126 162 75 202 196 230 65 223 182 240 216 163 36 231 163 192 39 216 187 182
         181 2 6 67 181 218 89 225 146 158 194 59 233 115 139 157 160 49 194 140 121
         54 204 36 19 167 244 201 171 74 134 128 59 73 107 239 228 69 228 53 163 217
         150 133)
       #+(and windows x86)
       #(92 128 89 162 167 212 205 88 165 186 68 27 136 188 237 238 117 158 121 79 177
         138 150 229 40 232 221 74 181 254 114 188 88 197 202 136 118 60 152 215 197 7
         229 202 95 170 157 66 154 60 187 247 184 142 188 41 36 207 171 95 85 8 113 70)))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokegen))))
  (format NIL "-DCMAKE_BUILD_TYPE=Release ~
               -DCMAKE_INSTALL_PREFIX=~s"
          (uiop:native-namestring (first (asdf:output-files 'install-op system)))))

(defun smokegen-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "smokegen" :defaults (relative-dir path "bin"))))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (list* (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "generate" "bin"))
         (call-next-method)))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokegen))))
  (loop for dir in '(#+unix #p"/usr"
                     #+unix #p"/usr/local"
                     #+(and x86-64 windows) #p"C:/Program Files/smokegenerator/"
                     #+(and x86 windows) #p"C:/Program Files (x86)/smokegenerator/")
        when (smokegen-on-path-p dir)
        return (values (list dir) T)
        finally (return (append (call-next-method)
                                (list (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "install" "bin")))))))
