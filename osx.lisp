#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

;; Detect PM
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Yes you can have multiple ones at the same time, but
  ;; we only allow one to simplify the code everywhere.
  (setf *features* (remove-if (lambda (a) (find a '(:osx-ports :osx-brew :osx-fink))) *features*))
  (pushnew (cond ((uiop:directory-exists-p #p"/opt/local/lib/") :osx-ports)
                 ((uiop:directory-exists-p #p"/usr/local/Cellar/") :osx-brew)
                 ((uiop:directory-exists-p #p"/sw/lib/") :osx-fink)
                 (T :osx-nopm)) *features*))

#+osx-ports (status 0 "Detected MacPorts.")
#+osx-brew (status 0 "Detected Homebrew.")
#+osx-fink (status 0 "Detected Fink.")
