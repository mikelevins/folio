;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       tools for manipulating sequences, series, and generators
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:net.bardcode.folio.sequences
  (:use #:cl)
  (:export
   #:add-first
   #:add-last
   #:any
   #:by
   #:drop
   #:element
   #:empty?
   #:filter
   #:first
   #:generate
   #:last
   #:length
   #:map
   #:next-last
   #:partition
   #:reduce
   #:rest
   #:reverse
   #:scan
   #:second
   #:some?
   #:take
   #:take-by))



