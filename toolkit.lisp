#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *max-cpus* most-positive-fixnum)

(defun externalize (thing)
  (typecase thing
    (list (mapcar #'externalize thing))
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

(defun read-path ()
  (pathname (read)))

(defun check-file-exists (file)
  (unless (probe-file file)
    (error "The file is required but does not exist:~%  ~s" file)))

(defmacro with-retry-restart ((name report &rest report-args) &body body)
  (let ((tag (gensym "RETRY-TAG"))
        (return (gensym "RETURN"))
        (stream (gensym "STREAM")))
    `(block ,return
       (tagbody
          ,tag (restart-case
                   (return-from ,return
                     (progn ,@body))
                 (,name ()
                   :report (lambda (,stream) (format ,stream ,report ,@report-args))
                   (go ,tag)))))))

(defun qt-libs-cache-directory ()
  (uiop:pathname-directory-pathname
   (asdf:output-file 'asdf:compile-op (asdf:find-component (asdf:find-system :qt-libs) "qt-libs"))))

(defun platform ()
  #+windows :win
  #+linux :lin
  #+darwin :mac
  #-(or windows linux darwin)
  (error "This platform is unsupported."))

(defun arch ()
  #+x86-64 :64
  #+x86 :32
  #-(or x86-64 x86)
  (error "This architecture is unsupported."))
