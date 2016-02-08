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
              ;; First two lines are the file itself again.
              (cddr lines)))))

(defun dylib-set-options (pathname &key name dependencies rpaths add-rpaths remove-rpaths)
  (assert (evenp (length dependencies)) ()
          "Must supply a balanced number of DEPENDENCY and NEW pairs.")
  (assert (evenp (length rpaths)) ()
          "Must supply a balanced number of RPATH and NEW pairs.")
  (with-chdir (pathname)
    (run "install_name_tool ~@[-id ~s ~]~
                            ~{-change ~s ~s ~}~
                            ~{-rpath ~s ~s ~}~
                            ~{-add_rpath ~s ~}~
                            ~{-delete_rpath ~s ~}~
                            ~s" name dependencies rpaths add-rpaths remove-rpaths (filename pathname))))

(defun dylib-set-install-name (pathname name)
  (dylib-set-options pathname :name name))

(defun dylib-set-dependency-name (pathname &rest pairs)
  (dylib-set-options pathname :dependencies pairs))

;; Attempts to find a good match by a distance function.
(defun find-similar (pathname files)
  (cl-ppcre:register-groups-bind (NIL name) ("(lib)?([^.]*)" (filename pathname))
    (cadar
     (sort (loop for file in files
                 for filename = (filename file)
                 for position = (search name filename)
                 when position
                 collect (list (+ position (- (length filename) (length name)))
                               file))
           #'< :key #'first))))

(defun fix-dylib-paths (pathname &optional (replacements (uiop:directory-files pathname)))
  ;; Primitively change relative paths to use @loader-path and matching name in dir.
  (let ((files (remove "dylib" replacements :key #'pathname-type :test-not #'string=))
        (pairs ()))
    (dolist (dep (dylib-dependencies pathname))
      (unless (search "@loader_path/" dep)
        (let* ((path (pathname dep))
               (new (when (or (uiop:relative-pathname-p path)
                              (find dep '("/opt/local/" "/usr/local/" "/sw/lib/") :test (lambda (a b) (search b a))))
                      (let ((corresponding (find-similar path files)))
                        (if corresponding
                            (format NIL "@loader_path/~a" (uiop:native-namestring
                                                           (relative-pathname pathname corresponding)))
                            dep)))))
          (when new
            (status 0 "Replacing ~a's dependency ~s with ~s."
                    pathname dep new)
            (push new pairs)
            (push dep pairs)))))
    ;; Primitively set the install name to the filename and set the new deps.
    (dylib-set-options pathname :name (filename pathname) :dependencies pairs))
  pathname)

(defun fix-dylib-collection (files)
  (dolist (file files)
    (fix-dylib-paths file files)))
