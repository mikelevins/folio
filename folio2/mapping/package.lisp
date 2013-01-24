;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tools for manipulating finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.folio.mapping
  (:use #:cl)
  (:export
   #:contains-key?
   #:contains-value?
   #:get-key
   #:keys
   #:merge
   #:put-key
   #:table
   #:vals))




