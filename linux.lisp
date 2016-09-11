#|
 This file is a part of Qtools
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun patchelf (pathname &rest args)
  (uiop:run-program (append (list "patchelf") (mapcar #'externalize args) (list (externalize pathname))) :output :string))

(defun ldlib-dependencies (pathname)
  (with-chdir (pathname)
    (cl-ppcre:split "\\n" (patchelf pathname "--print-needed"))))

(defun ldlib-set-options (pathname &key name dependencies rpath interpreter)
  (with-chdir (pathname)
    (when name
      (patchelf pathname "--set-soname" name))
    (when dependencies
      (dolist (dependency dependencies)
        (destructuring-bind (from to) dependency
          (cond ((not to)
                 (patchelf pathname "--remove-needed" (externalize from)))
                ((not from)
                 (patchelf pathname "--add-needed" (externalize to)))
                ((not (equal from to))
                 (patchelf pathname "--replace-needed" (externalize from) (externalize to)))))))
    (when rpath
      (patchelf pathname "--set-rpath" (externalize rpath)))
    (when interpreter
      (patchelf pathname "--set-interpreter" (externalize interpreter)))))

(defun ldlib-set-dependency-name (pathname &rest pairs)
  (so-set-options pathname :dependencies pairs))

(defun fix-ldlib-paths (pathname &optional (files (uiop:directory-files pathname)))
  (let ((dependencies ()))
    (dolist (dep (ldlib-dependencies pathname))
      (let* ((path (pathname dep))
             (new (let ((corresponding (find (determine-shared-library-name path) files :key #'determine-shared-library-name :test #'string-equal)))
                    (when corresponding
                      (format NIL "~a" (file-name corresponding))))))
        (when new
          (status 0 "Replacing ~a's dependency ~s with ~s."
                  pathname dep new)
          (push (list dep new) dependencies))))
    (ldlib-set-options pathname :dependencies dependencies :rpath ".")))

(defun fix-ldlib-collection (files)
  (dolist (file files)
    (fix-ldlib-paths file files)))
