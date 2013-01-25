;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       sequence functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.sequences)

;;; function cycle
;;;
;;; (cycle seq) => cycling series
;;; ---------------------------------------------------------------------
;;; returns a series of integers starting with START and continuing
;;; forever. Each succeeding integer differes from the previous one by BY.

(defun cycle (s)
  (let ((len (length s))
        (state 0))
    (series:scan-fn 't 
                    (lambda ()(elt s state))
                    (lambda (i)
                      (setf state (mod (1+ state) len))
                      (elt s state)))))

;;; function range-from
;;;
;;; (range-from start &key (by 1)) -> integer series
;;; ---------------------------------------------------------------------
;;; returns a series of integers starting with START and continuing
;;; forever. Each succeeding integer differes from the previous one by BY.

(defun range-from (n &key (by 1))
  (series:scan-range :from n :by by))


