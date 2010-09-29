;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional associative arrays
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.MAPS"
  (:use :as :cl :fn)
  (:import-from :seq "EMPTY?")
  (:nicknames "MAP")
  (:shadow "MERGE")
  (:export "ASSOCIATE" "CONTAINS-KEY?" "DISSOCIATE" "GET-KEY" "KEYS" 
           "MAKE" "MAKE-AS" "MERGE" "ORDERED-MAP" "VALS" "ZIPMAP"))

(in-package :map)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the maps subsystem provides a uniform mapping API for alists,
;;; fset:map values, and a simple ordered map implementation that
;;; maintains its key/value pairs in the order they were added.

;;; =====================================================================
;;; private utils
;;; =====================================================================

(defun %plist->alist (plist)
  (if (null plist)
      nil
      (if (null (cdr plist))
          (error "Malformed key/value list in ~S" plist)
          (acons (car plist) (cadr plist)
                 (%plist->alist (cddr plist))))))

;;; =====================================================================
;;; classes
;;; =====================================================================

;;; a naive implementation of a map that maintains its keys and values
;;; in the order that they were added. if an existing key is updated,
;;; it is moved to the end of the entries in the (newly-created)
;;; updated object.

;;; for now, this type stores its entries in an alist. if the class
;;; turns out to be useful enough and inefficient enough, we can
;;; revisit this design.

(defclass ordered-map ()
  ((entries :accessor %map-entries :initform nil :initarg :entries)))

(defmethod %find-entry ((om ordered-map) key &key (test 'eql))
  (assoc key (%map-entries om) :test test))

;;; =====================================================================
;;; AS methods
;;; =====================================================================

(defmethod as ((class (eql 'cl:list)) (thing fset:map) &key &allow-other-keys)
  (fset:convert 'cl:list thing))

(defmethod as ((class (eql 'fset:map)) (thing fset:map) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'fset:map)) (thing cl:list) &key &allow-other-keys)
  (fset:convert 'fset:map thing))

(defmethod as ((class (eql 'cl:list)) (thing ordered-map) &key &allow-other-keys)
  (copy-tree (%map-entries thing)))

(defmethod as ((class (eql 'fset:map)) (thing ordered-map) &key &allow-other-keys)
  (as 'fset:map (as 'list thing)))

(defmethod as ((class (eql 'ordered-map)) (thing fset:map) &key &allow-other-keys)
  (as 'ordered-map (as 'list thing)))

(defmethod as ((class (eql 'ordered-map)) (thing cl:list) &key &allow-other-keys)
  (assert (seq:every? 'listp thing)(thing)
          "AS cannot convert a value to type MAP:ORDERED-MAP: ~S" thing)
  (make-instance 'ordered-map :entries thing))

(defmethod as ((class (eql 'ordered-map)) (thing ordered-map) &key &allow-other-keys)
  thing)

;;; =====================================================================
;;; MAP API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; associate
;;; ---------------------------------------------------------------------

(defmethod associate ((m cl:list) key val &key &allow-other-keys)
  (acons key val m))

(defmethod associate ((m fset:map) key val  &key &allow-other-keys)
  (fset:with m key val))

(defmethod associate ((m ordered-map) key val &key (test 'eql) &allow-other-keys)
  (if (%find-entry m key :test test)
      (make-instance 'ordered-map 
                     :entries (seq:add-last (seq:filter (fn (e)(not ($ test key (car e))))
                                                        (%map-entries m))
                                            (cons key val)))
      (make-instance 'ordered-map :entries (seq:add-last (%map-entries m) (cons key val)))))

;;; ---------------------------------------------------------------------
;;; contains-key?
;;; ---------------------------------------------------------------------

(defmethod contains-key? ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (and (assoc key m :test test)
       t))

(defmethod contains-key? ((m fset:map) key &key &allow-other-keys)
  (fset:domain-contains? m key))

(defmethod contains-key? ((m ordered-map) key &key (test 'eql) &allow-other-keys)
  (and (%find-entry m key :test test)
       t))

;;; ---------------------------------------------------------------------
;;; dissociate
;;; ---------------------------------------------------------------------

(defmethod dissociate ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (remove-if (lambda (entry)
               (funcall test key (car entry))) 
             m))

(defmethod dissociate ((m fset:map) key &key &allow-other-keys)
  (fset:less m key))

(defmethod dissociate ((m ordered-map) key &key (test 'eql) &allow-other-keys)
  (if (%find-entry m key :test test)
      (make-instance 'ordered-map 
                     :entries (remove-if (lambda (entry) (funcall test key (car entry))) 
                                         (%map-entries m)))
      m))

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(defmethod empty? ((m fset:map))
  (fset:empty? m))

(defmethod empty? ((m ordered-map))
  (empty? (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; get-key
;;; ---------------------------------------------------------------------

(defmethod get-key ((m cl:list) key &key (test 'eql) (default nil) &allow-other-keys)
  (let ((entry (assoc key m :test test)))
    (if entry (cdr entry) default)))

(defmethod get-key ((m fset:map) key &key (default nil) &allow-other-keys)
  (let ((mdefault (fset:map-default m))
        (found (fset:@ m key)))
    (case (fset:compare mdefault found)
      ((:equal) default)
      (t found))))

(defmethod get-key ((m ordered-map) key &key (test 'eql) (default nil) &allow-other-keys)
  (let ((entry (%find-entry m key :test test)))
    (if entry (cdr entry) default)))

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(defmethod keys ((m cl:list))(seq:image 'car m))
(defmethod keys ((m fset:map))(fset:domain m))
(defmethod keys ((m ordered-map))(keys (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; make
;;; ---------------------------------------------------------------------

(defun make (&rest plist)
  (if (null plist)
      nil
      (acons (car plist) (cadr plist)
             (apply 'make (cddr plist)))))

(defun make-as (type &rest args)
  (case type
    ((cl:list) (apply 'make args))
    ((ordered-map) (make-instance 'ordered-map :entries (apply 'make args)))
    ((fset:map) (as 'fset:map (apply 'make args)))
    (t (error "Unrecognized map type: ~S" type))))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(defmethod merge ((m1 cl:list) m2 &key (test 'eql) &allow-other-keys)
  (if (empty? m2)
      m1
      (let* ((m2 (as 'list m2))
             (k (caar m2))
             (v (cdar m2)))
        (acons k v
               (merge (seq:filter (fn (e) (not ($ test k (car e)))) 
                              m1) 
                      (cdr m2))))))

(defmethod merge ((m1 fset:map) m2 &key &allow-other-keys)
  (if (empty? m2)
      m1
      (fset:map-union m1 (as 'fset:map m2))))

(defmethod merge ((m1 ordered-map) m2 &key (test 'eql) &allow-other-keys)
  (if (empty? m2)
      m1
      (let* ((m2-pairs (as 'list m2))
             (m1-pairs (seq:filter (fn (e) (not (seq:some? (fn (p) ($ test (car e)(car p)))
                                                           m2-pairs)))
                                   (%map-entries m1))))
        (make-instance 'ordered-map :entries (seq:concat m1-pairs m2-pairs)))))

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(defmethod vals ((m cl:list))(seq:image 'cdr m))
(defmethod vals ((m fset:map))(fset:range m))
(defmethod vals ((m ordered-map))(seq:image 'cdr (%map-entries m)))

;;; ---------------------------------------------------------------------
;;; zipmap
;;; ---------------------------------------------------------------------

(defmethod zipmap ((ks cl:list)(vs cl:list))
  (seq:image (lambda (k v) (cons k v)) ks vs))
