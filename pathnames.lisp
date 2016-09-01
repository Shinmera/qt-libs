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
  (format NIL "~a~@[.~a~]" (pathname-name pathname) (pathname-type pathname)))

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

(defun relative-pathname (from to)
  (let ((from (merge-pathnames from))
        (to (merge-pathnames to)))
    (unless (equal (pathname-host from) (pathname-host to))
      (error "Cannot relativise pathnames across hosts."))
    (unless (equal (pathname-device from) (pathname-device to))
      (error "Cannot realtivise pathnames across devices."))
    (let ((from-dir (copy-list (pathname-directory from)))
          (to-dir (copy-list (pathname-directory to)))
          (final-dir (list :relative)))
      (loop for a = (car from-dir)
            for b = (car to-dir)
            while (and (equal a b) from-dir to-dir)
            do (pop from-dir) (pop to-dir))
      (loop repeat (length from-dir)
            do (push :up final-dir))
      (loop for to in (reverse to-dir)
            do (push to final-dir))
      (make-pathname :directory (nreverse final-dir) :defaults to))))

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

(defun directory-name (file)
  (car (last (pathname-directory file))))

(defun copy-directory-files (dir to &key replace)
  (dolist (file (merge-pathnames uiop:*wild-file* dir))
    (copy-file file to :replace replace)))

(defun copy-file (file to &key replace)
  (cond ((uiop:directory-pathname-p file)
         (let ((to (relative-dir to (directory-name file))))
           (ensure-directories-exist to)
           (copy-directory-files file to)))
        (T
         (let ((to (make-pathname :name (pathname-name file)
                                  :type (pathname-type file)
                                  :defaults to)))
           (when (or replace (not (uiop:file-exists-p to)))
             (uiop:copy-file file to))))))

(defun shared-library-file (&rest args &key host device directory name version defaults)
  (declare (ignore host device directory version))
  (apply #'make-pathname :type #+windows "dll" #+darwin "dylib" #-(or windows darwin) "so"
                         :name (or (and name #-windows (concatenate 'string "lib" name))
                                   (pathname-name defaults))
                         args))

(defun make-shared-library-files (names defaults &key (key #'identity))
  (remove-if-not #'uiop:file-exists-p
                 (mapcar (lambda (name)
                           (uiop:resolve-symlinks
                            (first (or (directory (funcall key (shared-library-file :name name :defaults defaults)))
                                       #+(or osx-brew osx-fink) (directory (funcall key (merge-pathnames name defaults)))))))
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
             (cl-ppcre:register-groups-bind (NIL name) ("^(lib)?(.+)$" (pathname-name pathname))
               #+windows name #-windows (concatenate 'string "lib" name))))))
