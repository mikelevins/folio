;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.asd
;;;; Project:       folio - Bard features for Common Lisp
;;;; Purpose:       system definition for folio
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :net.bardcode.folio.applying
  :serial t
  :description "combinators and other conveniences for working with functions"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "applying"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.boxing
  :serial t
  :description "wrapping values in mutable containers"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "boxing"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.box-syntax
  :serial t
  :description "syntactic sugar for boxes"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.boxing)
  :components ((:module "boxing"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.constructing
  :serial t
  :description "uniform tools for constructing values"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "constructing"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.converting
  :serial t
  :description "uniform tools for converting values from one type to another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "converting"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.listing
  :serial t
  :description "tools for manipulating sequences, series, and generators"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "listing"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.sequence-syntax
  :serial t
  :description "syntacitc sugar for sequences"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.listing)
  :components ((:module "listing"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.mapping
  :serial t
  :description "tools for manipulating finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "mapping"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.table-syntax
  :serial t
  :description "syntactic sugar for finite maps"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "mapping"
                        :serial t
                        :components ((:file "package")
                                     (:file "syntax")))))

(asdf:defsystem :net.bardcode.folio.ordering
  :serial t
  :description "sorting values into stable orders"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "ordering"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.pairing
  :serial t
  :description "associating one value with another"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "pairing"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.sets
  :serial t
  :description "treating sequences as sets"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "sets"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio.set-syntax
  :serial t
  :description "syntactic sugar for sets"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series :net.bardcode.folio.sets)
  :components ((:module "sets"
                        :serial t
                        :components ((:file "package")))))


(asdf:defsystem :net.bardcode.folio.text
  :serial t
  :description "functional operations on text strings"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series)
  :components ((:module "text"
                        :serial t
                        :components ((:file "package")))))

(asdf:defsystem :net.bardcode.folio
  :serial t
  :description "umbrella system for loading all folio systems"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:fset :series
                     :net.bardcode.folio.applying
                     :net.bardcode.folio.boxing 
                     :net.bardcode.folio.box-syntax
                     :net.bardcode.folio.constructing
                     :net.bardcode.folio.converting
                     :net.bardcode.folio.listing
                     :net.bardcode.folio.sequence-syntax
                     :net.bardcode.folio.mapping
                     :net.bardcode.folio.table-syntax
                     :net.bardcode.folio.ordering
                     :net.bardcode.folio.pairing
                     :net.bardcode.folio.sets
                     :net.bardcode.folio.set-syntax
                     :net.bardcode.folio.text))

(defun load-folio ()
  (asdf:oos 'asdf:load-op :net.bardcode.folio))

;;; (load-folio)
