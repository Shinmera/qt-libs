#|
 This file is a part of Qtools
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass foreign-library ()
  ((name :initform NIL :accessor name)
   (base-directory :initarg :base-directory :accessor base-directory)
   (build-directory :initarg :build-directory :accessor build-directory)
   (install-directory :initarg :install-directory :accessor install-directory)))

(defmethod initialize-instance :after ((library foreign-library) &key name base-directory build-directory install-directory)
  (unless name
    (setf (name library) (string-downcase (class-name (class-of library)))))
  (unless base-directory
    (setf (base-directory library)
          (merge-pathnames (make-pathname :directory `(:relative ,(name library)))
                           (qt-libs-cache-directory))))
  (unless build-directory
    (setf (build-directory library) (merge-pathnames "build/" (base-directory library))))
  (unless install-directory
    (setf (install-directory library) (merge-pathnames "install/" (base-directory library)))))

(defgeneric stage (stage foreign-library &key force &allow-other-keys))
(defgeneric output-files (foreign-library))

(defmethod stage :around (stage (library foreign-library) &key)
  (with-retry-restart (retry "Retry stage ~a for ~a" stage library)
    (status 0 "Running stage ~a for ~a" stage library)
    (call-next-method)
    library))

(defmethod stage ((stage (eql :clean)) (library foreign-library) &key)
  (when (uiop:directory-exists-p (build-directory library))
    (uiop:delete-directory-tree (build-directory library) :validate (constantly T)))
  (when (uiop:directory-exists-p (install-directory library))
    (uiop:delete-directory-tree (install-directory library) :validate (constantly T))))

(defmethod output-files ((library foreign-library))
  (uiop:directory* (merge-pathnames uiop:*wild-file* (install-directory library))))

(defclass download-library (foreign-library)
  ((sources-archive :initarg :sources-archive :accessor sources-archive)
   (binaries-archive :initarg :binaries-archive :accessor binaries-archive)))

(defmethod initialize-instance :after ((library download-library) &key sources-archive binaries-archive)
  (unless sources-archive
    (setf (sources-archive library) (merge-pathnames "sources.zip" (base-directory library))))
  (unless binaries-archive
    (setf (binaries-archive library) (merge-pathnames "binaries.zip" (base-directory library)))))

(defgeneric sources-url (download-library))
(defgeneric binaries-url (download-library &key platform arch))

(defmethod stage :around ((stage (eql :download-sources)) (library download-library) &key force)
  (when (or force (not (uiop:file-exists-p (sources-archive library))))
    (ensure-directories-exist (base-directory library))
    (call-next-method)))

(defmethod stage :around ((stage (eql :download-binaries)) (library download-library) &key force)
  (when (or force (not (uiop:file-exists-p (binaries-archive library))))
    (ensure-directories-exist (base-directory library))
    (call-next-method)))

(defmethod stage ((stage (eql :download-sources)) (library download-library) &key)
  (download-file (sources-url library)
                 (sources-archive library)))

(defmethod stage ((stage (eql :prepare-sources)) (library download-library) &key force)
  (stage :download-sources library :force force)
  (check-file-exists (sources-archive library))
  (when force (uiop:delete-directory-tree (build-directory library) :validate (constantly T) :if-does-not-exist :ignore))
  (ensure-directories-exist (build-directory library))
  (extract-archive (sources-archive library) (build-directory library) :strip-folder T))

(defmethod stage ((stage (eql :download-binaries)) (library download-library) &key (platform (platform)) (arch (arch)))
  (download-file (binaries-url library :platform platform :arch arch)
                 (binaries-archive library)))

(defmethod stage ((stage (eql :install-binaries)) (library download-library) &key force)
  (stage :download-binaries library :force force)
  (check-file-exists (binaries-archive library))
  (when force (uiop:delete-directory-tree (install-directory library) :validate (constantly T) :if-does-not-exist :ignore))
  (ensure-directories-exist (install-directory library))
  (extract-archive (binaries-archive library) (install-directory library)))

(defmethod stage :after ((stage (eql :clean)) (library download-library) &key archives)
  (when (and archives (probe-file (sources-archive library)))
    (delete-file (sources-archive library)))
  (when (and archives (probe-file (binaries-archive library)))
    (delete-file (binaries-archive library))))

(defclass github-library (download-library)
  ((url :initform NIL :initarg :url :accessor url)
   (tag :initform NIL :initarg :tag :accessor tag)))

(defmethod initialize-instance :after ((library github-library) &key url)
  (unless url
    (setf (url library) (format NIL "https://github.com/Shinmera/~a" (or (name library)
                                                                         (error "NAME required."))))))

(defmethod sources-url ((library github-library))
  (format NIL "~a/archive/~a.zip"
          (or (url library) (error "No URL set."))
          (or (tag library) (error "No TAG set."))))

(defmethod binaries-url ((library github-library) &key (platform (platform)) (arch (arch)))
  (format NIL "~a/releases/download/~a/~(~a~)~a-~a.zip"
          (or (url library) (error "No URL set."))
          (or (tag library) (error "No TAG set."))
          platform arch (name library)))

(defclass build-library (foreign-library)
  ())

(defmethod stage :around ((stage (eql :prepare-sources)) (library build-library) &key force)
  (when (or force (not (uiop:directory-exists-p (build-directory library))))
    (call-next-method)))

(defmethod stage :around ((stage (eql :compile-sources)) (library build-library) &key force)
  (let ((build-fragment (merge-pathnames ".fragment" (build-directory library))))
    (when (or force (not (uiop:file-exists-p build-fragment)))
      (stage :prepare-sources library :force force)
      (check-file-exists (build-directory library))    
      (call-next-method)
      (with-open-file (s build-fragment :direction :output :if-exists :supersede)))))

(defmethod stage :around ((stage (eql :install-sources)) (library build-library) &key force)
  (when (or force (not (uiop:directory-exists-p (install-directory library))))
    (stage :compile-sources library :force force)
    (ensure-directories-exist (install-directory library))
    (call-next-method)))

(defclass make-build-library (build-library)
  ((make-file :initarg :make-file :initform "Makefile" :accessor make-file)
   (make-flags :initarg :make-flags :initform (list "-j" (max 1 (1- (cpu-count)))) :accessor make-flags)))

(defmethod stage ((stage (eql :prepare-sources)) (library make-build-library) &key)
  (when (next-method-p) (call-next-method)))

(defmethod stage ((stage (eql :compile-sources)) (library make-build-library) &key)
  (check-prerequisite "Make" "gnumake" "make")
  (check-file-exists (merge-pathnames (make-file library) (build-directory library)))
  (with-chdir ((build-directory library))
    (run-here "make -f ~s~{ ~s~}" (make-file library) (make-flags library))))

(defmethod stage ((stage (eql :install-sources)) (library make-build-library) &key)
  (check-file-exists (merge-pathnames (make-file library) (build-directory library)))
  (with-chdir ((build-directory library))
    (run-here "make install -f ~s~{ ~s~}" (make-file library) (make-flags library))))

(defclass cmake-build-library (make-build-library)
  ((cmake-flags :initarg :cmake-flags :initform () :accessor cmake-flags)))

(defmethod stage ((stage (eql :prepare-sources)) (library cmake-build-library) &key)
  (when (next-method-p) (call-next-method))
  (check-prerequisite "CMake" "cmake")
  (check-file-exists (merge-pathnames "CMakeLists.txt" (build-directory library)))
  (with-chdir ((build-directory library))
    (run-here "cmake .~{ ~s~}" (cmake-flags library))))

(defclass checksummed-library (foreign-library)
  ((checksums :initarg :checksums :initform NIL :accessor checksums)))

(defgeneric expected-checksum (checksummed-library archive &key &allow-other-keys))

(defmethod expected-checksum ((library checksummed-library) (archive (eql :sources)) &key)
  (getf (checksums library) :sources))

(defmethod expected-checksum ((library checksummed-library) (archive (eql :binaries)) &key (platform (platform)) (arch (arch)))
  (third (find (list platform arch) (checksums library) :key #'butlast :test #'equal)))

(defmethod stage :after ((stage (eql :download-sources)) (library checksummed-library) &key)
  (let ((expected (expected-checksum library :sources)))
    (if expected
        (check-checksum (sources-archive library) expected)
        (warn "No checksum for source archive of ~a" library))))

(defmethod stage :after ((stage (eql :download-binaries)) (library checksummed-library) &key platform arch)
  (let ((expected (expected-checksum library :binaries :platform platform :arch arch)))
    (if expected
        (check-checksum (binaries-archive library) expected)
        (warn "No checksum for ~a ~a binary archive of ~a" platform arch library))))

(defclass locally-available-library (foreign-library)
  ((local-file-cache :initform :unsaved :accessor local-file-cache)))

(defgeneric find-local-files (locally-available-library))
(defgeneric locally-available-p (locally-available-library))

(defmethod find-local-files :around ((library locally-available-library))
  (let ((cache (local-file-cache library)))
    (if (eql cache :unsaved)
        (setf (local-file-cache library) (call-next-method))
        cache)))

(defmethod locally-available-p ((library locally-available-library))
  (not (null (find-local-files library))))

(defmethod stage ((stage (eql :download-sources)) (library locally-available-library) &key))

(defmethod stage ((stage (eql :prepare-sources)) (library locally-available-library) &key)
  (unless (locally-available-p library)
    (error "~a cannot be found locally." (name library))))

(defmethod stage ((stage (eql :install-sources)) (library locally-available-library) &key)
  (stage :prepare-sources library)
  (ensure-directories-exist (install-directory library))
  (dolist (file (find-local-files library))
    (copy-file file (install-directory library))))
