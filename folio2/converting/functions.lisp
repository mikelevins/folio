;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       uniform tools for converting values from one type to another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.converting)

;;; function as
;;;
;;; (as type val) => an instance of type
;;; ---------------------------------------------------------------------
;;; returns a value equivalent to VAL whose type is TYPE

