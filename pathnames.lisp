#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun filetype (pathname)
  (let* ((type (pathname-type pathname))
         (pos (position #\. type :from-end T)))
    (if pos (subseq type (1+ pos)) type)))

(defun filename (pathname)
  (format NIL "~a.~a" (pathname-name pathname) (pathname-type pathname)))

(defun relative-dir (relative &rest subdirs)
  (loop for sub in subdirs
        for dir = (merge-pathnames (uiop:ensure-directory-pathname sub)
                                   (uiop:ensure-directory-pathname relative))
        then (merge-pathnames (uiop:ensure-directory-pathname sub) dir)
        finally (return dir)))

(defun upwards-file (file)
  (cond ((uiop:directory-pathname-p file)
         (let ((dirname (car (last (pathname-directory file))))
               (rootpath (copy-list (butlast (pathname-directory file) 2))))
           (setf (cdr (last rootpath)) (list dirname))
           (make-pathname :directory rootpath :defaults file)))
        (T
         (let ((rootpath (copy-list (butlast (pathname-directory file)))))
           (make-pathname :directory rootpath :defaults file)))))

(defmacro with-chdir ((to) &body body)
  (let ((current (gensym "CURRENT")))
    `(let ((,current (uiop:getcwd)))
       (unwind-protect
            (progn
              (uiop:chdir
               (uiop:pathname-directory-pathname
                (ensure-directories-exist ,to)))
              ,@body)
         (uiop:chdir ,current)))))

(defmacro with-temp-file ((name pathname) &body body)
  `(let ((,name ,pathname))
     (unwind-protect
          (progn ,@body)
       (uiop:delete-file-if-exists ,name))))

(defun shared-library-file (&rest args &key host device directory name version defaults)
  (declare (ignore host device directory version))
  (apply #'make-pathname :type #+windows "dll" #+darwin "dylib" #-(or windows darwin) "so"
                         :name (or (and name #-windows (concatenate 'string "lib" name))
                                   (pathname-name defaults))
                         args))

(defun make-shared-library-files (names defaults &key (key #'identity))
  (remove-if-not #'uiop:file-exists-p
                 (mapcar (lambda (name)
                           (funcall
                            key
                            (uiop:resolve-symlinks
                             (shared-library-file :name name :defaults defaults))))
                         names)))

(defun determine-shared-library-type (pathname)
  (cond ((search ".so." (pathname-name pathname))
         "so")
        (T (or (pathname-type pathname)
               #+darwin "dylib"
               #+unix "so"
               #+windows "dll"))))

(defun determine-shared-library-name (pathname)
  (cond ((search ".so." (pathname-name pathname))
         (subseq (pathname-name pathname) 0 (search ".so." (pathname-name pathname))))
        (T
         (or (cl-ppcre:register-groups-bind (name) ("^(.+)\\.\\d\\.\\d\\.\\d$" (pathname-name pathname)) name)
             (pathname-name pathname)))))
