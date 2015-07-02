#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun setenv (envvar new-value)
  #+sbcl (sb-posix:setenv envvar new-value 1)
  #+ccl (ccl:setenv envvar new-value T)
  #+ecl (ext:setenv envvar new-value)
  #-(or sbcl ccl ecl) (warn "Don't know how to perform SETENV.~
                           ~&Please set the environment variable ~s to ~s to ensure proper operation."
                            envvar new-value)
  new-value)

(defun get-path (&optional (envvar "PATH"))
  (cl-ppcre:split #+windows ";+" #-windows ":+" (uiop:getenv envvar)))

(defun set-path (paths &optional (envvar "PATH"))
  (setenv envvar (etypecase paths
                   (string paths)
                   (list (format NIL (load-time-value (format NIL "~~{~~a~~^~a~~}" #+windows ";" #-windows ":")) paths)))))

(defun pushnew-path (path &optional (envvar "PATH"))
  (let ((path (etypecase path
                (pathname (uiop:native-namestring path))
                (string path)))
        (paths (get-path envvar)))
    (pushnew path paths :test #'string=)
    (set-path paths envvar)))
