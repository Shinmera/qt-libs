#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *max-cpus* most-positive-fixnum)

(defun externalize (thing)
  (typecase thing
    (list thing)
    (string thing)
    (pathname (uiop:native-namestring thing))
    (T (princ-to-string thing))))

(defun status (n string &rest format-args)
  (format T "~&~a ~a~%"
          (case n (0 ">") (1 " ->") (2 " ==>") (T "  >>>"))
          (apply #'format NIL string format-args)))

(defun run (string &rest format-args)
  (let ((program (apply #'format NIL string (mapcar #'externalize format-args))))
    (uiop:run-program program :output T :error-output T)))

(defun run-here (string &rest format-args)
  (let ((program (apply #'format NIL string (mapcar #'externalize format-args))))
    (status 1 "Running ~a" program)
    (uiop:run-program program :output T :error-output T)))

(defun ensure-system (system &optional (package system))
  (unless (find-package package)
    (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning))
      #-quicklisp (asdf:load-system system)
      #+quicklisp (ql:quickload system))))

(defun application-available-p (&rest alternatives)
  (zerop (nth-value 2 (uiop:run-program (format NIL "~{command -v ~s~^ || ~}" alternatives) :ignore-error-status T))))

(defun test-prerequisite (name &rest alternatives)
  (with-simple-restart (continue "I know what I'm doing, skip this test.")
    (loop until (if (apply #'application-available-p alternatives)
                    T
                    (with-simple-restart (retry "I installed it now, test again.")
                      (error "~a is required, but could not be found. Please ensure it is installed properly." name))))))

(defun cpu-count ()
  (min (or (parse-integer (uiop:run-program "nproc" :ignore-error-status T :output :string) :junk-allowed T)
           2)
       *max-cpus*))

(defun url-filetype (url)
  (subseq url (1+ (or (position #\. url :start (or (position #\/ url :from-end T) 0))
                      (error "Unable to detect filetype for ~s" url)))))

(defun project-url (project version &key type)
  (ecase type
    (:git (project-git-url project))
    (:compiled (project-release-url project version))
    (:sources (project-sources-url project version))))

(defun project-git-url (project)
  (format NIL "https://github.com/Shinmera/~a.git" project))

(defun project-sources-url (project version)
  (format NIL "https://github.com/Shinmera/~a/archive/~a.zip" project version))

(defun project-release-url (project version)
  (format NIL "https://github.com/Shinmera/~a/releases/download/~a/~a~a-~a.zip"
          project version
          #+linux "lin" #+darwin "mac" #+windows "win" #-(or linux darwin windows) (error "Platform not supported.")
          #+x86-64 "64" #+x86 "32" #-(or x86-64 x86) (error "Platform not supported.")
          project))
