;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional sequences
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.SEQUENCES"
  (:use :cl :as :fn)
  (:nicknames "SEQ")
  (:shadow "FIND" "INTERSECTION" "LENGTH" "POSITION" "REDUCE" "REVERSE"
           "SEQUENCE" "SORT" "UNION")
  (:export "CONCAT" "CONTAINS?" "DIFFERENCE" "DROP" "DROP-WHILE" "ELEMENT" "EMPTY?" "EVERY?" "FILTER"
           "FIND" "HEAD" "IMAGE" "INTERLEAVE" "INTERPOSE" "INTERSECTION" "LENGTH"
           "MAKE" "MAKE-AS"
           "PARTITION" "POSITION" "RANGE" "REDUCE" "REPEAT" "REVERSE"
           "SEQUENCE" "SLICE" "SOME?" "SORT" "TAIL" "TAKE" "TAKE-WHILE"
           "UNION" "UNIQUE" "ZIP"))

(in-package :seq)

;;; =====================================================================
;;; utils
;;; =====================================================================

(defmethod classname-for-sequence ((c cl:list)) 'cl:list)
(defmethod classname-for-sequence ((c cl:vector)) 'cl:vector)
(defmethod classname-for-sequence ((c cl:string)) 'cl:string)
(defmethod classname-for-sequence ((c fset:seq)) 'fset:seq)
(defmethod classname-for-sequence ((c fset:set)) 'fset:set)

;;; =====================================================================
;;; AS methods
;;; =====================================================================

(defmethod as ((class (eql 'cl:vector)) (thing fset:seq) &key &allow-other-keys)
  (fset:convert 'cl:vector thing))

(defmethod as ((class (eql 'fset:seq)) (thing cl:vector) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'cl:list)) (thing fset:seq) &key &allow-other-keys)
  (fset:convert 'cl:list thing))

(defmethod as ((class (eql 'fset:seq)) (thing cl:list) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'cl:string)) (thing fset:seq) &key &allow-other-keys)
  (if (fset::every 'characterp thing)
      (string (as 'vector thing))
      (format nil "~S" thing)))

(defmethod as ((class (eql 'fset:seq)) (thing cl:string) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'string)) (thing fset:seq) &key &allow-other-keys)
  (if (fset::every 'characterp thing)
      (as 'string (as 'vector thing))
      (format nil "~S" thing)))

(defmethod as ((class (eql 'fset:seq)) (thing fset:seq) &key &allow-other-keys)
  thing)

;;; =====================================================================
;;; sequence API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; concat
;;; ---------------------------------------------------------------------

(defmethod concat (s0 s1)
  (concatenate (classname-for-sequence s0)
               s0 (as (classname-for-sequence s0) s1)))

(defmethod concat ((s0 fset:seq) s1)
  (fset:concat s0 (as 'fset:seq s1)))

;;; ---------------------------------------------------------------------
;;; contains?
;;; ---------------------------------------------------------------------

(defmethod contains? ((seq cl:sequence) x &key (test 'eql) &allow-other-keys) 
  (some (fn (v) ($ test v x)) seq))

(defmethod contains? ((seq fset:seq) x &key (test 'eql) &allow-other-keys)
  (fset:find x seq :test test))

;;; ---------------------------------------------------------------------
;;; difference
;;; ---------------------------------------------------------------------

(defmethod difference (s0 s1 &key (test 'eql))
  (as (classname-for-sequence s0)
   (cl:set-difference (as 'list s0)
                      (as 'list s1)
                      :test test)))

;;; ---------------------------------------------------------------------
;;; drop
;;; ---------------------------------------------------------------------

(defmethod drop (n s &key from-end?)
  (if from-end?
      (fset:subseq s 0 (- (cl:length s) 3))
      (fset:subseq s n)))

;;; ---------------------------------------------------------------------
;;; drop-while
;;; ---------------------------------------------------------------------

(defmethod drop-while (pred s &key from-end?)
  (let ((index (fset:position-if-not pred s :from-end from-end?)))
    (let ((start (if from-end? 0 index))
          (end (if from-end? (1+ index) (fset:size s))))
      (fset:subseq s start end))))

;;; ---------------------------------------------------------------------
;;; element
;;; ---------------------------------------------------------------------

(defmethod element (s n) (fset:@ s n))

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(defmethod empty? (s) (fset:empty? s))

;;; ---------------------------------------------------------------------
;;; every?
;;; ---------------------------------------------------------------------

(defmethod every? (pred s &rest more-sequences)
  (apply 'fset::every `(,pred ,s ,@more-sequences)))

;;; ---------------------------------------------------------------------
;;; filter
;;; ---------------------------------------------------------------------

(defmethod filter (pred s)
  (as (classname-for-sequence s)
      (fset:filter pred (as 'fset:seq s))))

(defmethod filter (pred (s fset:seq))
  (fset:filter pred s))

;;; ---------------------------------------------------------------------
;;; find
;;; ---------------------------------------------------------------------

(defmethod find (pred s &key from-end?)
  (fset:find-if pred s :from-end from-end?))

;;; ---------------------------------------------------------------------
;;; head
;;; ---------------------------------------------------------------------

(defmethod head (s)(fset:@ s 0))

;;; ---------------------------------------------------------------------
;;; image
;;; ---------------------------------------------------------------------

(defun image (fn s &rest more-sequences)
  (let ((ss (cons (as 'cl:list s) 
                  (mapcar (lambda (m) (as 'cl:list m))
                          more-sequences)))) 
    (as (classname-for-sequence s)
        (apply 'mapcar fn ss))))

;;; ---------------------------------------------------------------------
;;; interleave
;;; ---------------------------------------------------------------------

(defun interleave (s0 s1)
  (as (classname-for-sequence s0)
      (apply 'append
             (as 'cl:list
                 (image (lambda (u v) (list u v))
                        s0 s1)))))

;;; ---------------------------------------------------------------------
;;; interpose
;;; ---------------------------------------------------------------------

(defmethod interpose (elem s)
  (as (classname-for-sequence s) 
      (drop 1 (interleave (repeat (length s) elem) s))))

;;; ---------------------------------------------------------------------
;;; intersection
;;; ---------------------------------------------------------------------

(defun intersection (s0 s1 &key (test 'eql))
  (as (classname-for-sequence s0)
      (cl:intersection (as 'list s0)
                       (as 'list s1)
                       :test test)))

;;; ---------------------------------------------------------------------
;;; length
;;; ---------------------------------------------------------------------

(defmethod length (s)(fset:size s))

;;; ---------------------------------------------------------------------
;;; make
;;; ---------------------------------------------------------------------

(defun make (&rest args)(fset:convert 'fset:seq args))

(defun make-as (type &rest args)
  (case type
    ((cl:list) args)
    ((cl:vector) (as 'cl:vector args))
    ((cl:string) (as 'cl:string args))
    ((fset:seq) (as 'fset:seq args))
    (t (error "Unrecognized sequence type: ~S" type))))

;;; ---------------------------------------------------------------------
;;; partition
;;; ---------------------------------------------------------------------

(defmethod partition (n s &key step)
  (let ((step (or step n)))
    (if (<= (length s) n)
        (sequence s)
        (concat (sequence (take n s))
                (partition n (drop step s) :step step)))))

;;; ---------------------------------------------------------------------
;;; position
;;; ---------------------------------------------------------------------

(defmethod position (pred s)(fset:position-if pred s))

;;; ---------------------------------------------------------------------
;;; range
;;; ---------------------------------------------------------------------

(defmethod range (start end)
  (loop for i from start to (1- end) collect i))

;;; ---------------------------------------------------------------------
;;; reduce
;;; ---------------------------------------------------------------------

(defmethod reduce (fn s &key initial-value)
  (fset:reduce fn s :initial-value initial-value))

;;; ---------------------------------------------------------------------
;;; repeat
;;; ---------------------------------------------------------------------

(defmethod repeat (n item)
  (make-array n :initial-element item))

;;; ---------------------------------------------------------------------
;;; reverse
;;; ---------------------------------------------------------------------

(defmethod reverse (s)(fset:reverse s))

;;; ---------------------------------------------------------------------
;;; sequence
;;; ---------------------------------------------------------------------

(defun sequence (&rest elements)
  (as 'fset:seq elements))

;;; ---------------------------------------------------------------------
;;; slice
;;; ---------------------------------------------------------------------

(defmethod slice (s start &optional end)
  (fset:subseq s start end))

;;; ---------------------------------------------------------------------
;;; some?
;;; ---------------------------------------------------------------------

(defmethod some? (pred s &rest more-sequences)
  (apply 'fset::some `(,pred ,s ,@more-sequences)))

;;; ---------------------------------------------------------------------
;;; sort
;;; ---------------------------------------------------------------------

(defmethod sort (pred s)(fset:stable-sort s pred))

;;; ---------------------------------------------------------------------
;;; tail
;;; ---------------------------------------------------------------------

(defmethod tail (s) (subseq s 1))

(defmethod tail ((s cl:list)) 
  (cdr s))

(defmethod tail ((s fset:seq)) (fset:less-first s))

;;; ---------------------------------------------------------------------
;;; take
;;; ---------------------------------------------------------------------

(defmethod take (n s &key from-end?)
  (if from-end?
      (fset:subseq s n (- (cl:length s) 3))
      (fset:subseq s 0 n)))

;;; ---------------------------------------------------------------------
;;; take-while
;;; ---------------------------------------------------------------------

(defmethod take-while (pred s &key from-end?)
  (let ((index (fset:position-if-not pred s :from-end from-end?)))
    (let ((start (if from-end? (1+ index) 0))
          (end (if from-end? (fset:size s) index)))
      (fset:subseq s start end))))

;;; ---------------------------------------------------------------------
;;; union
;;; ---------------------------------------------------------------------

(defmethod union (s0 s1 &key (test 'eql))
  (unique (concat s0 s1) :test test))

;;; ---------------------------------------------------------------------
;;; unique
;;; ---------------------------------------------------------------------

(defun unique (s &key (test 'eql))
  (as (classname-for-sequence s)
      (remove-duplicates (as 'list s) :test test)))

;;; ---------------------------------------------------------------------
;;; unzip
;;; ---------------------------------------------------------------------

(defmethod unzip (s)
  (let ((sprime (as 'cl:list s)))
    (values (as (classname-for-sequence s)(mapcar 'head sprime))
            (as (classname-for-sequence s)(mapcar 'tail sprime)))))

;;; ---------------------------------------------------------------------
;;; zip
;;; ---------------------------------------------------------------------

(defmethod zip (s0 s1)
  (image (lambda (u v)
           (cons u v))
         s0 s1))
