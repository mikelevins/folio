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

(in-package :net.bardcode.folio.functions)

;;; function compose
;;;
;;; (compose f1 f2 ... fk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (f1 (f2 (...(fk args)))). All arguments f1..fk must accept and return 
;;; the same number of values

;;; function conjoin
;;;
;;; (conjoin p1 p2 ... pk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (and (p1 arg)(p2 arg)...(pk arg))

;;; function disjoin
;;;
;;; (disjoin p1 p2 ... pk) => fn
;;; ---------------------------------------------------------------------
;;; returns a function whose effect is the same as that of applying
;;; (or (p1 arg)(p2 arg)...(pk arg))

;;; function flip
;;;
;;; (flip f1) => f2
;;; ---------------------------------------------------------------------
;;; given an argument f1 of the form (lambda (a b)...), returns a
;;; function of the form (lambda (b a) ...). Except for the order of the
;;; arguments a and b, the new function is identical to the old.

;;; function partial
;;;
;;; (partial f1 arg1..argk) => f2
;;; ---------------------------------------------------------------------
;;; partially applies the function f1 to the arguments arg1..argk,
;;; returning a left section of f1. In other words, if f1 accepts
;;; arguments a, b, c, and d, then (partial f1 0 1) returns an f2 in
;;; which a is bound to 0 and b is bound to 1. f2 then requires
;;; two arguments. Evaluating (f2 2 3) binds c and d to 2 and 3,
;;; respectively, then computes the same result as if we had
;;; originally called (f1 0 1 2 3).


;;; function rpartial
;;;
;;; (rpartial f1 arg1..argk) => f2
;;; ---------------------------------------------------------------------
;;; partially applies the function f1 to the arguments arg1..argk,
;;; returning a right section of f1. In other words, if f1 accepts
;;; arguments a, b, c, and d, then (rpartial f1 2 3) returns an f2 in
;;; which c is bound to 2 and d is bound to 3. f2 then requires
;;; two arguments. Evaluating (f2 0 1) binds a and b to 0 and 1,
;;; respectively, then computes the same result as if we had
;;; originally called (f1 0 1 2 3).



