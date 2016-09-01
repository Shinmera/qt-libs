#|
 This file is a part of Qtools
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass foreign-library ()
  ((name :initform NIL :accessor name)))

(defgeneric download-sources (foreign-library to))
(defgeneric download-binaries (foreign-library to &key platform arch))
(defgeneric prepare-sources (foreign-library at))
(defgeneric compile-sources (foreign-library at))
(defgeneric install-sources (foreign-library from to))

(defclass github-library (foreign-library)
  ((url :initform NIL :initarg :url :accessor url)
   (tag :initform NIL :initarg :tag :accessor tag)))

(defmethod initialize-instance :after ((library github-library) &key url)
  (unless url
    (setf url (format NIL "https://github.com/Shinmera/~a" (or (name library)
                                                               (error "NAME required."))))))

(defgeneric sources-url (github-library))
(defgeneric binaries-url (github-library &key platform arch))

(defmethod sources-url ((library github-library))
  (format NIL "~a/archive/~a.zip" (url library) (or (tag library)
                                                    (error "No TAG set."))))

(defmethod binaries-url ((library github-library) &key (platform (platform)) (arch (arch)))
  (format NIL "~a/releases/download/~a/~(~a~)~a-~a.zip"
          (url library) (tag library) platform arch (name library)))

(defmethod download-sources ((library github-library) to)
  (download-file (sources-url library) to))

(defmethod download-binaries ((library github-library) to &rest args &key platform arch)
  (declare (ignore platform arch))
  (download-file (apply #'binaries-url library args) to))

(defclass make-build-library (foreign-library)
  ((make-file :initarg :make-file :initform "Makefile" :accessor make-file)
   (make-target :initarg :make-target :initform NIL :accessor make-target)
   (make-flags :initarg :make-flags :initform (list "-j" (max 1 (1- (cpu-count)))) :accessor make-flags)))

(defmethod prepare-sources ((library make-build-library) at))

(defmethod compile-sources ((library make-build-library) at)
  (test-prerequisite "Make" "gnumake" "make")
  (check-file-exists (merge-pathnames (make-file library) at))
  (with-chdir (at)
    (run-here "make ~a -f ~s~{ ~s~}"
              (make-target library) (make-file library) (make-flags library))))

(defclass cmake-build-library (make-build-library)
  ((cmake-build-directory :initarg :cmake-build-directory :initform "build/" :accessor cmake-build-directory)
   (cmake-flags :initarg :cmake-flags :initform () :accessor cmake-flags)))

(defmethod prepare-sources ((library cmake-build-library) at)
  (test-prerequisite "CMake" "cmake")
  (check-file-exists (merge-pathnames "CMakeLists.txt" at))
  (let ((dir (merge-pathnames (cmake-build-directory library) at)))
    (with-chdir (dir)
      (run-here "cmake ~a~{ ~s~}" at (cmake-flags library)))))

(defclass checksummed-library (foreign-library)
  ((checksums :initarg :checksums :initform NIL :accessor checksums)))

(defgeneric expected-checksum (checksummed-library archive &key &allow-other-keys))

(defmethod expected-checksum ((library checksummed-library) (archive (eql :sources)) &key)
  (getf archive (checksums library)))

(defmethod expected-checksum ((library checksummed-library) (archive (eql :binaries)) &key (platform (platform)) (arch (arch)))
  (third (find (list platform arch) (checksums library) :key #'butlast :test #'equal)))

(defun check-checksum (file checksum)
  (let ((received (checksum-file file)))
    (unless (equalp received checksum)
      (cerror "I am sure that this is fine."
              "SHA3 file mismatch for ~s!~
             ~&Expected ~a~
             ~&Got      ~a"
              file (checksum-string checksum) (checksum-string received)))))

(defmethod download-sources :after ((library checksummed-library) to)
  (check-checksum to (expected-checksum library :sources)))

(defmethod download-binaries :after ((library checksummed-library) to &key platform arch)
  (check-checksum to (expected-checksum library :binaries :platform platform :arch arch)))

(defclass locally-available-library (foreign-library)
  ())

(defgeneric find-local-files (locally-available-library))
(defgeneric locally-available-p (locally-available-library))

(defmethod locally-available-p ((library locally-available-library))
  (not (null (find-local-files library))))

(defmethod install ((library locally-available-library) to &key force)
  (cond ((locally-available-p library)
         (dolist (file (find-local-files library))
           (let ((to (make-pathname :name (pathname-name file)
                                    :type (pathname-type file)
                                    :defaults to)))
             (when (or force (not (uiop:file-exists-p to)))
               (uiop:copy-file file to)))))
        ((next-method-p)
         (call-next-method))
        (T
         (error "Have not found local files for ~a and don't know how to build."
                library))))

(defclass cached-build-library (foreign-library)
  ((base-directory :initarg :base-directory :accessor base-directory)
   (build-directory :initarg :build-directory :accessor build-directory)
   (install-directory :initarg :install-directory :accessor install-directory)
   (sources-archive :initarg :sources-archive :accessor sources-archive)
   (binaries-archive :initarg :binaries-archive :accessor binaries-archive)))

(defmethod initialize-instance :after ((library cached-build-library) &key base-directory build-directory install-directory sources-archive binaries-archive)
  (unless base-directory
    (setf (base-directory library)
          (merge-pathnames (make-pathname :directory `(:relative ,(name library)))
                           (qt-libs-cache-directory))))
  (unless build-directory
    (setf (build-directory library) (merge-pathnames "build/" (base-directory library))))
  (unless install-directory
    (setf (install-directory library) (merge-pathnames "install/" (base-directory library))))
  (unless sources-archive
    (setf (sources-archive library) (merge-pathnames "sources.zip" (base-directory library))))
  (unless binaries-archive
    (setf (binaries-archive library) (merge-pathnames "binaries.zip" (base-directory library)))))

(defgeneric run-stage (cached-build-library stage &key &allow-other-keys))
(defgeneric install (cached-build-library to &key &allow-other-keys))

(defmethod run-stage :around ((library cached-build-library) stage &key force)
  (declare (ignore force))
  (with-retry-restart (retry "Retry running ~a." stage)
    (call-next-method)))

(defmethod run-stage ((library cached-build-library) (stage (eql :download-sources)) &key force)
  (when (or force (not (uiop:file-exists-p (sources-archive library))))
    (ensure-directories-exist (base-directory library))
    (download-sources library (sources-archive library))))

(defmethod run-stage ((library cached-build-library) (stage (eql :download-binaries)) &key force (platform (platform)) (arch (arch)))
  (when (or force (not (uiop:file-exists-p (binaries-archive library))))
    (ensure-directories-exist (base-directory library))
    (download-binaries library (binaries-archive library) :platform platform :arch arch)
    (extract-archive (binaries-archive library) (install-directory library))))

(defmethod run-stage ((library cached-build-library) (stage (eql :prepare)) &key force)
  (let ((source-archive (sources-archive library))
        (build (build-directory library)))
    (run-stage library :download-sources :force force)
    (check-file-exists source-archive)
    (when (and force (uiop:directory-exists-p build))
      (uiop:delete-directory-tree build :validate (constantly T)))
    (ensure-directories-exist build)
    (extract-archive source-archive build :strip-folder T)
    (prepare-sources library build)))

(defmethod run-stage ((library cached-build-library) (stage (eql :compile)) &key force)
  (run-stage library :prepare :force force)
  (check-file-exists (build-directory library))
  (when (or force (not (uiop:directory-exists-p (install-directory library))))
    (compile-sources library (build-directory library))
    (install-sources library (build-directory library)
                     (install-directory library))))

(defmethod run-stage ((library cached-build-library) (stage (eql :install)) &key to force)
  (check-file-exists (install-directory library))
  (copy-directory-files (install-directory library) to :replace force))

(defmethod run-stage ((library cached-build-library) (stage (eql :clean)) &key archives)
  (when (and archives (probe-file (sources-archive library)))
    (delete-file (sources-archive library)))
  (when (and archives (probe-file (binaries-archive library)))
    (delete-file (binaries-archive library)))
  (when (probe-file (build-directory library))
    (uiop:delete-directory-tree (build-directory library) :validate (constantly T)))
  (when (probe-file (build-directory library))
    (uiop:delete-directory-tree (install-directory library) :validate (constantly T))))

(defmethod install ((library cached-build-library) to &key (method :binaries) force (platform (platform)) (arch (arch)))
  (ecase method
    (:sources
     (run-stage library :compile :force force))
    (:binaries
     (run-stage library :download-binaries :force force :platform platform :arch arch)))
  (run-stage library :install :to to :force force))
