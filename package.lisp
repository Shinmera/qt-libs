#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-lib-generator
  (:use #:cl)
  (:nicknames #:org.shirakumo.qtools.libs.generator)
  ;; archives.lisp
  (:export
   #:clone
   #:checksum-string
   #:checksum-file
   #:download-file
   #:safely-download-file
   #:extract-zip-archive
   #:extract-tar-archive
   #:extract-archive)
  ;; commonqt.lisp
  (:export
   #:fix-commonqt-pro-file)
  ;; dylib.lisp
  (:export
   #:dylib-dependencies
   #:dylib-set-install-name
   #:dylib-set-dependency-name
   #:fix-dylib-paths
   #:fix-dylib-collection)
  ;; pathnames.lisp
  (:export
   #:filetype
   #:filename
   #:relative-dir
   #:upqards-file
   #:with-chdir
   #:with-temp-file
   #:shared-library-file
   #:make-shared-lirbary-files
   #:determine-shared-library-type
   #:determine-shared-library-name)
  ;; qt4.lisp
  (:export)
  ;; setenv.lisp
  (:export
   #:setenv
   #:get-path
   #:set-path
   #:pushnew-path)
  ;; smokegen.lisp
  (:export)
  ;; smokeqt.lisp
  (:export)
  ;; system.lisp
  (:export
   #:build-system-op
   #:download-op
   #:generate-op
   #:install-op
   #:build-system
   #:make-build-system
   #:cmake-build-system
   #:origin
   #:shared-library-files
   #:install-system
   #:clean-system)
  ;; toolkit.lisp
  (:export
   #:*max-cpus*
   #:externalize
   #:status
   #:run
   #:run-here
   #:ensure-system
   #:application-available-p
   #:test-prerequisite
   #:cpu-count
   #:url-filetype
   #:project-url
   #:project-git-url
   #:project-sources-url
   #:project-release-url))
