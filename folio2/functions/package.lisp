;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators and other conveniences 
;;;;                for working with functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.folio.functions
  (:use #:cl)
  (:export
   #:$
   #:^
   #:->
   #:compose
   #:conjoin
   #:disjoin
   #:flip
   #:partial
   #:rpartial))


