#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass build-system-op (asdf:operation)
  ())

(defmethod asdf:perform :before ((op build-system-op) c)
  (status 2 (asdf:action-description op c)))

(defclass source-type-dependant-op (build-system-op)
  ((source-type :initarg :source-type :initform :sources :accessor source-type)))

;; I hate ASDF.
(defmethod asdf/operation:operation-original-initargs ((op source-type-dependant-op))
  `(:source-type ,(source-type op)))

(defclass download-op (source-type-dependant-op #+:asdf3.1 asdf:non-propagating-operation)
  ())

(defmethod asdf:action-description ((op download-op) c)
  (format nil "~@<downloading ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op download-op) c)
  NIL)

(defclass generate-op (build-system-op asdf:selfward-operation asdf:sideway-operation)
  ((asdf:selfward-operation :initform 'download-op :allocation :class)
   (asdf:sideway-operation :initform 'install-op :allocation :class)))

(defmethod asdf:action-description ((op generate-op) c)
  (format nil "~@<generating ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op generate-op) c)
  NIL)

(defclass install-op (source-type-dependant-op asdf:selfward-operation)
  ((asdf:selfward-operation :initform NIL :accessor selfward-op)))

(defmethod initialize-instance :after ((op install-op) &key)  
  (if (eql (source-type op) :compiled)
      (setf (selfward-op op) (asdf:make-operation 'download-op :source-type (source-type op)))
      (setf (selfward-op op) (asdf:make-operation 'generate-op))))

(defmethod asdf:action-description ((op install-op) c)
  (format nil "~@<installing ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op install-op) c)
  NIL)


(defclass build-system (asdf:system)
  ((proj-version :initarg :pversion :accessor version)))

(defgeneric origin (system &key type))

(defmethod origin ((system build-system) &key (type :sources))
  (project-url (asdf:component-name system) (version system) :type type))

(defgeneric checksum (system &key type))

(defmethod checksum ((system build-system) &key type)
  (declare (ignore system type))
  NIL)

(defgeneric shared-library-files (system))

(defmethod shared-library-files ((name T))
  (shared-library-files (asdf:find-system name)))

(defmethod shared-library-files ((system build-system))
  (mapcar #'uiop:resolve-symlinks
          (uiop:directory-files (first (asdf:output-files 'install-op system))
                                (shared-library-file :defaults uiop:*wild-file*))))

(defmethod asdf:component-pathname ((system build-system))
  (relative-dir (call-next-method) (asdf:component-name system)))

(defmethod asdf:input-files ((op download-op) (system build-system))
  (list))

(defmethod asdf:perform ((op download-op) (system build-system))
  (let* ((type (source-type op))
         (origin (origin system :type type)))
    (when origin
      (ecase type
        (:git
         (clone origin (first (asdf:output-files op system))))
        ((:sources :compiled)
         (with-temp-file (archive (make-pathname :name (format NIL "~a-archive" (asdf:component-name system))
                                                 :type (url-filetype origin) :defaults (uiop:temporary-directory)))
           (safely-download-file origin archive (checksum system :type type))
           (extract-archive archive (uiop:pathname-directory-pathname
                                     (first (asdf:output-files op system)))
                            :strip-folder (eql type :sources))))))))

(defmethod asdf:output-files ((op download-op) (system build-system))
  (list (uiop:ensure-directory-pathname "source")))

(defmethod asdf:input-files ((op generate-op) (system build-system))
  (asdf:output-files 'download-op system))

(defmethod asdf:perform ((op generate-op) (system build-system))
  (error "Need to implement ASDF:PERFORM on (~a ~a)" op system))

(defmethod asdf:output-files ((op generate-op) (system build-system))
  (list (uiop:ensure-directory-pathname "generate")))

(defmethod asdf:input-files ((op install-op) (system build-system))
  (asdf:output-files 'generate-op system))

(defmethod asdf:perform ((op install-op) (system build-system))
  (error "Need to implement ASDF:PERFORM on (~a ~a)" op system))

(defmethod asdf:perform :around ((op install-op) (system build-system))
  (if (eql (source-type op) :compiled)
      (let ((dir (relative-dir (first (asdf:output-files op system)) "lib")))
        (ensure-directories-exist dir)
        (dolist (file (uiop:directory-files (first (asdf:output-files 'download-op system))))
          (uiop:copy-file file (make-pathname :directory (pathname-directory dir) :defaults file))))
      (call-next-method)))

(defmethod asdf:output-files ((op install-op) (system build-system))
  (list (uiop:ensure-directory-pathname "install")))

(defmethod asdf/plan:traverse-action ((plan asdf/plan:plan-traversal) op (c build-system) niip)
  (when (or (asdf/plan::plan-forced plan)
            (loop for file in (asdf:output-files op c) never (probe-file file)))
    (call-next-method)))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (system build-system))
  `(,@(call-next-method)
    (install-op ,system)))


(defclass make-build-system (build-system)
  ((make-flags :initarg :make-flags :initform NIL :accessor make-flags)
   (install-flags :initarg :install-flags :initform NIL :accessor install-flags)))

(defmethod asdf:input-files ((op generate-op) (system make-build-system))
  (list* (make-pathname :name "Makefile" :type NIL :defaults (car (last (call-next-method))))
         (call-next-method)))

(defvar *ld-library-path* (let ((env (uiop:getenv #-darwin "LD_LIBRARY_PATH"
                                                  #+darwin "DYLD_LIBRARY_PATH")))
                            (if env (list env) ())))
(defmethod asdf:perform ((op generate-op) (system make-build-system))
  (let ((makefile (first (asdf:input-files op system)))
        (preamble #+unix (format NIL "env ~:[DYLD~;LD~]_LIBRARY_PATH=\"~{~a~^:~}\" "
                                 #-darwin T #+darwin NIL *ld-library-path*)
                  #-unix ""))
    (run-here "~amake -j ~d -C ~s -f ~s~@[ ~a~]"
              preamble (max 1 (1- (cpu-count))) (uiop:pathname-directory-pathname makefile)
              (format NIL "~a~@[.~a~]" (pathname-name makefile) (pathname-type makefile))
              (make-flags system))))

(defmethod asdf:perform ((op install-op) (system make-build-system))
  (with-chdir ((car (last (asdf:input-files op system))))
    (run-here "make install~@[ ~a~]"
              (install-flags system))))


(defclass cmake-build-system (make-build-system)
  ((cmake-flags :initarg :cmake-flags :initform NIL :accessor cmake-flags)))

(defmethod asdf:output-files ((op download-op) (system cmake-build-system))
  (append (call-next-method)
          (unless (eql (source-type op) :compiled)
            (list (make-pathname :name "CMakeLists" :type "txt" :defaults (uiop:ensure-directory-pathname "source"))))))

(defmethod asdf:input-files ((op generate-op) (system cmake-build-system))
  (list (make-pathname :name "Makefile" :type NIL :defaults (car (last (asdf:output-files op system))))
        (make-pathname :name "CMakeLists" :type "txt" :defaults (car (last (asdf:output-files 'download-op system))))))

(defmethod asdf:perform ((op generate-op) (system cmake-build-system))
  (with-chdir ((car (last (asdf:output-files op system))))
    (run-here "cmake ~s~@[ ~a~]"
              (uiop:pathname-directory-pathname (second (asdf:input-files op system)))
              (cmake-flags system)))
  (call-next-method))



(asdf:defsystem :qt-build-prerequisites)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :qt-build-prerequisites))))
  #+darwin (pushnew-path "/opt/local/bin")
  (test-prerequisite "CMake" "cmake")
  (test-prerequisite "Make" "make")
  (test-prerequisite "C Compiler" "cc" "gcc")
  (test-prerequisite "tar" "tar"))

(defun show-plan (op system)
  (loop for (operation . component) in (asdf/plan:plan-actions (asdf:make-plan 'asdf:sequential-plan op (asdf:find-system system)))
        do (format T "~&~15a ~a~%" (type-of operation) (asdf:component-name component))))

;; We /really/ only want the install-op for this.
(defmethod asdf/plan:traverse-action (plan op (c (eql (asdf:find-system :qt-build-prerequisites))) niip)
  (typecase op
    (install-op (call-next-method))
    (T NIL)))


(defun install-system (system &rest args &key (source-type :compiled) force &allow-other-keys)
  (remf args :source-type)
  (when force (clean-system system))
  (apply #'asdf:operate (asdf:make-operation 'install-op :source-type source-type) system args))

(defun clean-system (system)
  (flet ((deldir (dir)
           (when (uiop:directory-exists-p dir)
             (uiop:delete-directory-tree dir :validate (constantly T)))))
    (deldir (first (asdf:output-files 'install-op system)))
    (deldir (first (asdf:output-files 'generate-op system)))
    (deldir (first (asdf:output-files 'download-op system))))
  system)
