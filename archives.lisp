#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun clone (origin target)
  (test-prerequisite "GIT" "git")
  (status 2 "Cloning ~a" origin)
  (run-here "git clone ~s ~s" origin target))

(defun checksum-string (vector)
  (with-output-to-string (*standard-output*)
    (map NIL (lambda (c) (write c :base 36)) vector)))

(defun checksum-file (target)
  (ensure-system :sha3)
  (funcall (find-symbol (string :sha3-digest-file) :sha3) target))

(defun download-file (url target)
  (status 1 "Downloading ~a to ~a" url target)
  (ensure-system :drakma)
  (with-open-file (output target :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
    (multiple-value-bind (input status) (funcall (find-symbol (string :http-request) :drakma) url :want-stream T)
      (unwind-protect
           (progn
             (unless (= status 200)
               (error "Bad status code: ~s" status))
             (loop for byte = (read-byte input NIL NIL)
                   while byte
                   do (write-byte byte output)))
        (close input))))
  target)

(defun safely-download-file (url target checksum)
  (loop do (download-file url target)
        until (cond (checksum
                     (with-simple-restart (retry "Retry downloading.")
                       (let ((file-checksum (checksum-file target)))
                         (unless (equalp checksum file-checksum)
                           (cerror "I am sure that this is fine."
                                   "SHA3 file mismatch for ~s!~
                                  ~&Expected ~a~
                                  ~&got      ~a"
                                   (uiop:native-namestring target) (checksum-string checksum) (checksum-string file-checksum)))
                         (status 1 "Checksum test passed")
                         T)))
                    (T (status 1 "No checksum available, skipping test.")
                       T)))
  target)

(defun extract-zip-archive (from to &key (strip-folder))
  (ensure-system :zip)
  (funcall (find-symbol (string :unzip) :zip) from to)
  (when strip-folder
    (let ((sub (first (uiop:subdirectories to))))
      (dolist (file (append (uiop:directory-files sub) (uiop:subdirectories sub)))
        (rename-file file (upwards-file file)))
      (uiop:delete-file-if-exists sub)))
  to)

(defun extract-tar-archive (from to &key (strip-folder))
  (test-prerequisite "tar" "tar")
  (status 2 "Extracting ~a" (uiop:native-namestring from))
  (run-here "tar ~@[--strip-components=1 ~*~] -xpf ~s -C ~s" strip-folder from to)
  to)

(defun extract-archive (from to &key (strip-folder))
  (let ((type (filetype from)))
    (cond ((or (string-equal type "gz")
               (string-equal type "xz"))
           (extract-tar-archive from to :strip-folder strip-folder))
          ((string-equal type "zip")
           (extract-zip-archive from to :strip-folder strip-folder))
          (T (error "Don't know how to extract ~s" from)))))
