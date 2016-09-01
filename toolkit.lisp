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

(defun read-path ()
  (pathname (read)))

(defun check-file-exists (file)
  (unless (probe-file file)
    (error "The file is required but does not exist:~%  ~s" file)))

(defun generate-checksum-func (system)
  (let ((system (etypecase system
                  (asdf:system (asdf:component-name system))
                  ((or string symbol) (string-downcase system)))))
    (format T "~&(defmethod checksum ((system (eql (asdf:find-system :~a))) &key type)
  (when (equal (version system) ~s)
    (case type" system (version (asdf:find-system system)))
    
    (flet ((file (short)
             (asdf:system-relative-pathname :qt-libs (format NIL "package/~a-~a.zip" short system))))
      ;; Source
      (when (uiop:file-exists-p (file "src"))
        (format T "~&      (:sources")
        (format T "~&       ~s" (checksum-file (file "src")))
        (format T ")"))
      ;; Compiled
      (format T "~&      (:compiled")
      (loop for (short os plaf) in '(("lin64" linux x86-64)
                                     ("mac64" darwin x86-64)
                                     ("win32" windows x86)
                                     ("win64" windows x86-64))
            do (when (uiop:file-exists-p (file short))
                 (format T "~&       #+(and ~(~a~) ~(~a~))" os plaf)
                 (format T "~&       ~s" (checksum-file (file short)))))
      (format T ")"))
    (format T ")))")))

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
