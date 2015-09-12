#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun dylib-dependencies (pathname)
  (with-chdir (pathname)
    (let ((lines (cl-ppcre:split "\\s*\\n\\s*" (uiop:run-program (format NIL "otool -L ~s" (filename pathname)) :output :string))))
      (mapcar (lambda (line)
                (cl-ppcre:register-groups-bind (name) ("^(.*) \\(compatibility version" line)
                  name))
              (cdr lines)))))

(defun dylib-set-install-name (pathname name)
  (with-chdir (pathname)
    (run "install_name_tool -id ~s ~s" name (filename pathname))))

(defun dylib-set-dependency-name (pathname dependency name)
  (with-chdir (pathname)
    (run "install_name_tool -change ~s ~s ~s" dependency name (filename pathname))))

;; Attempts to find a good match by a distance function.
(defun find-similar (pathname files)
  (cl-ppcre:register-groups-bind (NIL name) ("(lib)?(.*?)\\." (filename pathname))
    (cadar
     (sort (loop for file in files
                 for filename = (filename file)
                 for position = (search name filename)
                 when position
                 collect (list (+ position (- (length filename) (length name)))
                               file))
           #'< :key #'first))))

(defun fix-dylib-paths (pathname)
  ;; Primitively set the install name to the filename
  (dylib-set-install-name pathname (filename pathname))
  ;; Primitively change relative paths to use @loader-path and matching name in dir.
  (let ((files (remove "dylib" (uiop:directory-files pathname) :key #'pathname-type :test-not #'string=)))
    (dolist (dep (dylib-dependencies pathname))
      (let ((path (pathname dep)))
        (cond ((uiop:relative-pathname-p path)
               (dylib-set-dependency-name
                pathname dep
                (let ((corresponding (find-similar path files)))
                  (if corresponding
                      (format NIL "@loader_path/~a" (filename corresponding))
                      dep))))
              ((find dep '("/opt/local/" "/usr/local/" "/sw/lib/") :test (lambda (a b) (search b a)))
               (dylib-set-dependency-name
                pathname dep
                (let ((corresponding (find-similar path files)))
                  (if corresponding
                      (format NIL "@loader_path/~a" (filename corresponding))
                      dep))))))))
  pathname)
