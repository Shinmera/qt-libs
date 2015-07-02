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

;; This is stupid, but I can't be bothered to do better.
(defun find-similar (pathname files)
  (let ((stripped (cl-ppcre:register-groups-bind (NIL name) ("(lib)?(.*?)\\." (filename pathname)) name)))
    (loop for file in files
          when (search stripped (filename file))
          return file)))

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
              ((search "/opt/local/" dep)
               (dylib-set-dependency-name
                pathname dep
                (let ((corresponding (find-similar path files)))
                  (if corresponding
                      (format NIL "@loader_path/~a" (filename corresponding))
                      dep))))))))
  pathname)
