;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       system definition for folio
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun folio-root () (make-pathname :directory (pathname-directory loadpath))))

(defun find-asdf-systems ()
  (let ((sysdefs (directory (merge-pathnames "**/*.asd" (folio-root)))))
    (map 'list
         (lambda (s) (make-pathname :directory (pathname-directory s)))
         sysdefs)))

(defun init-asdf-registry ()
  (dolist (s (find-asdf-systems))
    (pushnew s asdf:*central-registry* :test 'equal)))

(in-package :cl-user)

(defun load-folio ()
  (init-asdf-registry)
  (asdf:oos 'asdf:load-op :as)
  (asdf:oos 'asdf:load-op :functions)
  (asdf:oos 'asdf:load-op :collections))
