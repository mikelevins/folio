;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       syntactic sugar
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.folio.functions)

;;; macro $
;;;
;;; ($ f &rest args) => vals
;;; ---------------------------------------------------------------------
;;; folio's application operator.
;;; if f is a function or generic function then $ is a synonym for
;;; FUNCALL. If f is a sequence, table, series, or generator, then
;;; it is a synonym for net.bardcode.folio.tables:GET-KEY

;;; macro ^
;;; 
;;; (^ (arg1..argk) expr1..exprk) => a function
;;; ---------------------------------------------------------------------
;;; A more compact synonym for LAMBDA. This macro is not intended as
;;; a replacement for LAMBDA, but as a convenience for cases in
;;; which the clarity of functional code benefits from compactness.

;;; macro ->
;;;
;;; (-> (arg1..argk) f1..fn) => val1..valk
;;; ---------------------------------------------------------------------
;;; pronounced "flow", -> is a convenience for writing programs in
;;; which a set of data values flows through a series of
;;; transformations. Each function f1..fn must accept and return k
;;; values, and the flow form returns k values. The inputs to f1 are
;;; the values arg1..argk; the inputs to f2 are the k values returned
;;; by f1; the inputs to f3 are the k values returned by f2, and so
;;; on. The flow operator greatly simplifies code in which a tuple of
;;; values undergoes a series of transformations that changes the 
;;; values, but not the size of the tuple. Many processes can be
;;; conveniently represented this way; for example, a virtual 
;;; machine can conveniently be represented as a tuple of registers
;;; and a set of operations that change their values.


