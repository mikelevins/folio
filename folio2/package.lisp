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

(defpackage :net.bardcode.folio
  (:nicknames :folio)
  (:use :cl :net.bardcode.folio.functions
        :net.bardcode.folio.boxes 
        :net.bardcode.folio.constructing
        :net.bardcode.folio.converting
        :net.bardcode.folio.sequences
        :net.bardcode.folio.tables
        :net.bardcode.folio.ordering
        :net.bardcode.folio.pairing
        :net.bardcode.folio.sets
        :net.bardcode.folio.text)
  (:export
   ;; boxes
   :@ :box :box? :set-box! :unbox
   ;; constructing
   :make
   ;; converting
   :as
   ;; functions
   :$ :^ :-> :compose :conjoin :disjoin :flip :partial :rpartial
   ;; ordering
   :greater? :greater-or-equal? :less? :less-or-equal? :order-by
   ;; pairs
   :left :pair :right
   ;; sequences
   :add-first :add-last :any :by :drop :element :empty? :filter 
   :first :generate :last :length :map :next-last :partition
   :reduce :rest :reverse :scan :second :some? :take :take-by
   ;; sets
   ;; tables
   :contains-key? :contains-value? :get-key :keys :merge-maps 
   :put-key :table :vals
   ;; text
   :join-text :split-text))


