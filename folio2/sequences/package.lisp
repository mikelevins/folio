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
   #:append
   #:coalesce
   #:drop
   #:drop-while
   #:element
   #:empty?
   #:every?
   #:filter
   #:first
   #:generate
   #:interleave
   #:interpose
   #:join
   #:last
   #:length
   #:map
   #:next-last
   #:partition
   #:position
   #:range
   #:range-from
   #:reduce
   #:repeat
   #:rest
   #:reverse
   #:scan
   #:second
   #:select
   #:sequence?
   #:shuffle
   #:slice
   #:some?
   #:sort
   #:split
   #:subsequence
   #:tails
   #:take
   #:take-by
   #:take-while
   #:unique
   #:unzip
   #:zip))



