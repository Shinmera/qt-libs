#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class cmake-build-system
  :pversion "qt-libs1.1.0"
  :depends-on (#-windows :qt-build-prerequisites))

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
       #(241 175 129 130 243 248 133 202 219 78 28 87 117 155 222 206 60 67 241 113
         199 165 226 63 25 252 119 75 254 62 96 89 3 211 9 139 167 118 127 202 50 158
         41 220 174 212 194 220 77 191 17 220 191 126 88 20 142 213 40 54 34 12 77 40)
       #+(and windows x86)
       #(75 237 36 0 220 69 85 213 92 11 11 99 168 198 247 169 200 14 147 71 106 191
         45 236 125 144 5 23 59 232 50 181 188 69 78 40 200 80 160 222 41 194 109 62
         145 88 212 236 150 232 45 195 110 93 76 225 124 188 164 157 62 143 192 201)))))

(defmethod cmake-flags ((system (eql (asdf:find-system :smokegen))))
  (format NIL "-DCMAKE_BUILD_TYPE=Release ~
               -DCMAKE_INSTALL_PREFIX=~s"
          (uiop:native-namestring (first (asdf:output-files 'install-op system)))))

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (list* (make-pathname :name "smokegen" :type NIL :defaults (relative-dir "generate" "bin"))
         (call-next-method)))

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
        finally (return (list (relative-dir "install" "lib")
                              (shared-library-file :name "smokebase" :defaults (relative-dir "install" "lib"))))))

(defmethod shared-library-files ((system (eql (asdf:find-system :smokegen))))
  (make-shared-library-files '("smokebase") (first (asdf:output-files 'install-op system))))
