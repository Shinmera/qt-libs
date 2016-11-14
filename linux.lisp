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

(defun ldlib-soname (pathname)
  (with-chdir (pathname)
    (first (cl-ppcre:split "\\n" (patchelf pathname "--print-soname")))))

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

(defun soname-assoc-list (files)
  (mapcar (lambda (a) (cons a (ldlib-soname a))) files))

(defun fix-ldlib-paths (pathname &optional (sonames (soname-assoc-list (uiop:directory-files pathname))))
  (let ((dependencies ()))
    (dolist (dep (ldlib-dependencies pathname))
      (let ((new (let ((corresponding (car (find (subseq (determine-shared-library-name dep) 3)
                                                 sonames :key #'cdr :test #'string-equal))))
                   (when corresponding
                     (file-name corresponding)))))
        (when new
          (status 0 "Replacing ~a's dependency ~s with ~s."
                  pathname dep new)
          (push (list dep new) dependencies))))
    (ldlib-set-options pathname :dependencies dependencies
                                :rpath "."
                                :name (file-name pathname))))

(defun fix-ldlib-collection (files)
  (let ((sonames (soname-assoc-list files)))
    (dolist (file files)
      (fix-ldlib-paths file sonames))))
