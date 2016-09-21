#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-lib-generator
  (:use #:cl #:pathname-utils)
  (:nicknames #:org.shirakumo.qtools.libs.generator)
  ;; build.lisp
  (:export
   #:foreign-library
   #:name
   #:base-directory
   #:build-directory
   #:install-directory
   #:stage
   #:output-files
   #:download-library
   #:sources-archive
   #:binaries-archive
   #:github-library
   #:url
   #:tag
   #:build-library
   #:make-build-library
   #:make-file
   #:make-flags
   #:cmake-build-library
   #:cmake-flags
   #:checksummed-library
   #:checksums
   #:expected-checksum
   #:locally-available-library
   #:local-file-cache
   #:find-local-files
   #:locally-available-p)
  ;; libcommonqt.lisp
  (:export
   #:libcommonqt
   #:smokeqt
   #:fix-commonqt-pro-file)
  ;; linux.lisp
  (:export
   #:patchelf
   #:ldlib-dependencies
   #:ldlib-soname
   #:ldlib-set-options
   #:ldlib-set-dependency-name
   #:fix-ldlib-paths
   #:fix-ldlib-collection)
  ;; osx
  (:export
   #:dylib-dependencies
   #:dylib-set-options
   #:dylib-set-install-name
   #:dylib-set-dependency-name
   #:find-similar
   #:fix-dylib-paths
   #:fix-dylib-collection)
  ;; qt4
  (:export
   #:qt4)
  ;; smokegen
  (:export
   #:smokegen)
  ;; smokeqt
  (:export
   #:smokeqt)
  ;; toolkit
  (:export
   #:*max-cpus*
   #:externalize
   #:status
   #:run-here
   #:ensure-system
   #:application-available-p
   #:check-prerequisite
   #:cpu-count
   #:check-file-exists
   #:qt-libs-cache-directory
   #:platform
   #:arch
   #:with-chdir
   #:copy-directory-files
   #:copy-file
   #:shared-library-file
   #:make-shared-library-files
   #:determine-shared-library-type
   #:determine-shared-library-name
   #:checksum-string
   #:checksum-file
   #:download-file
   #:extract-archive
   #:check-checksum
   #:setenv
   #:get-path
   #:set-path
   #:pushnew-path))
