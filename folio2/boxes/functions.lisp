;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators and other conveniences 
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.boxes)

;;; function compose
;;;
;;; (compose f1 f2 ... fk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (f1 (f2 (...(fk args)))). All arguments f1..fk must accept and return 
;;; the same number of values
