;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       functions that accept and produce
;;;;                sequences, series, generators, and streams
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.folio.sequences)

;;; ---------------------------------------------------------------------
;;; function add-first
;;; ---------------------------------------------------------------------
;;;
;;; (add-first x seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X prepended to the elements of
;;; SEQ

(defgeneric add-first (x seq))

;;; ---------------------------------------------------------------------
;;; function add-last
;;; ---------------------------------------------------------------------
;;;
;;; (add-last seq x) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence that contains X appended after the elements of
;;; SEQ

(defgeneric add-last (seq x))

;;; ---------------------------------------------------------------------
;;; function any
;;; ---------------------------------------------------------------------
;;;
;;; (any seq) => anything
;;; ---------------------------------------------------------------------
;;; returns an arbitrary element of seq. any chooses the element
;;; randomly

(defgeneric any (seq))

;;; ---------------------------------------------------------------------
;;; function append
;;; ---------------------------------------------------------------------
;;;
;;; (append seq &rest more-seqs) => seq'
;;; ---------------------------------------------------------------------

;;;(defgeneric append (seq &rest more-seqs))


;;; ---------------------------------------------------------------------
;;; function coalesce
;;; ---------------------------------------------------------------------
;;;
;;; (coalesce fn &rest seqs) => seq'
;;; ---------------------------------------------------------------------
;;; coalesce combines N sequences into one. SEQs is a set of N sequences.
;;; FN is a function that accepts N inputs and returns one output. Applying
;;; coalesce yields a single sequence, series, generator, or stream
;;; that is the sequence of values produced by applying FN to SEQs.

(defgeneric coalesce (fn &rest seqs))

;;; ---------------------------------------------------------------------
;;; function drop
;;; ---------------------------------------------------------------------
;;;
;;; (drop n seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a new sequence containing the elements of SEQ after the first
;;; N elements have been removed

(defgeneric drop (n seq))

;;; ---------------------------------------------------------------------
;;; function drop-while
;;; ---------------------------------------------------------------------
;;;
;;; (drop-while test seq) => seq'
;;; ---------------------------------------------------------------------

(defgeneric drop-while (test seq))

;;; ---------------------------------------------------------------------
;;; function element
;;; ---------------------------------------------------------------------
;;;
;;; (element seq n) => 
;;; ---------------------------------------------------------------------
;;; returns the element of SEQ at index N

(defgeneric element (seq n))

;;; ---------------------------------------------------------------------
;;; function empty?
;;; ---------------------------------------------------------------------
;;;
;;; (empty? seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric empty? (seq))

;;; ---------------------------------------------------------------------
;;; function every?
;;; ---------------------------------------------------------------------
;;;
;;; (every? test seq) => a boolean
;;; ---------------------------------------------------------------------
;;; returns true if SEQ contains no elements, and false otherwise

(defgeneric every? (test seq))

;;; ---------------------------------------------------------------------
;;; function filter
;;; ---------------------------------------------------------------------
;;;
;;; (filter test seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns those elements of SEQ for which TEST returns true

(defgeneric filter (test seq))

;;; ---------------------------------------------------------------------
;;; function first
;;; ---------------------------------------------------------------------
;;;
;;; (first seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the first element of SEQ

;;;(defgeneric first (seq))

;;; ---------------------------------------------------------------------
;;; function interleave
;;; ---------------------------------------------------------------------
;;;
;;; (interleave seq1 seq2) => seq3
;;; ---------------------------------------------------------------------

(defgeneric interleave (seq1 seq2))

;;; ---------------------------------------------------------------------
;;; function interpose
;;; ---------------------------------------------------------------------
;;;
;;; (interpose item seq) => anything
;;; ---------------------------------------------------------------------

(defgeneric interpose (cupola seq))

;;; ---------------------------------------------------------------------
;;; function join
;;; ---------------------------------------------------------------------
;;;
;;; (join cupola seq) => seq'
;;; ---------------------------------------------------------------------

(defgeneric join (cupola seq))

;;; ---------------------------------------------------------------------
;;; function last
;;; ---------------------------------------------------------------------
;;;
;;; (last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last element of SEQ

;;;(defgeneric last (seq))

;;; ---------------------------------------------------------------------
;;; function length
;;; ---------------------------------------------------------------------
;;;
;;; (length seq) => an integer
;;; ---------------------------------------------------------------------
;;; returns a count of the elements in SEQ

;;;(defgeneric length (seq))

;;; ---------------------------------------------------------------------
;;; function map
;;; ---------------------------------------------------------------------
;;;
;;; (map fn seq) => seq'
;;; ---------------------------------------------------------------------
;;; returns a sequence containing the values produced by applying
;;; FN to each element of SEQ

;;;(defgeneric map (fn seq))

;;; ---------------------------------------------------------------------
;;; function next-last
;;; ---------------------------------------------------------------------
;;;
;;; (next-last seq) => anything
;;; ---------------------------------------------------------------------
;;; returns the last-but-one element of seq

(defgeneric next-last (seq))

;;; ---------------------------------------------------------------------
;;; function partition
;;; ---------------------------------------------------------------------
;;;
;;; (partition seq &rest functions) => seq1 seq2 seq3...
;;; ---------------------------------------------------------------------

(defgeneric partition (seq &rest fns))


;;; function range-from
;;;
;;; (range-from start &key (by 1)) -> integer series
;;; ---------------------------------------------------------------------
;;; returns a series of integers starting with START and continuing
;;; forever. Each succeeding integer differes from the previous one by BY.

(defun range-from (n &key (by 1))
  (series:scan-range :from n :by by))

;;; function reduce
;;;
;;; () => 
;;; ---------------------------------------------------------------------

;;;(defgeneric reduce (fn &rest args))

;;; function repeat
;;;
;;; (repeat seq) => cycling series
;;; ---------------------------------------------------------------------
;;; returns the elements of seq, followed by the same elements in the same
;;; order, and repeating in that pattern forever

(defun repeat (s)
  (let ((len (length s))
        (state 0))
    (series:scan-fn 't 
                    (lambda ()(elt s state))
                    (lambda (i)
                      (setf state (mod (1+ state) len))
                      (elt s state)))))

;;; function rest
;;;
;;; () => 
;;; ---------------------------------------------------------------------

;;;(defgeneric rest (seq))

;;; function reverse
;;;
;;; () => 
;;; ---------------------------------------------------------------------

;;;(defgeneric reverse (seq))

;;; function scan
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defgeneric scan (seq))

;;; function second
;;;
;;; () => 
;;; ---------------------------------------------------------------------

;;;(defgeneric second (seq))

;;; function some?
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defgeneric some? (test seq))

;;; function take
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defgeneric take (n seq))

;;; function take-by
;;;
;;; () => 
;;; ---------------------------------------------------------------------

(defgeneric take-by (n advance seq))


