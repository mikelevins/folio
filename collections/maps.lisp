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
  (:use :as :cl)
  (:nicknames "MAP")
  (:shadow "MERGE")
  (:export "ASSOCIATE" "CONTAINS-KEY?" "DISSOCIATE" "GET-KEY" "KEYS" 
           "MAKE" "MAKE-AS" "MERGE" "VALS" "ZIPMAP"))

(in-package :map)

;;; TODO: add stably-ordered maps

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
;;; AS methods
;;; =====================================================================

(defmethod as ((class (eql 'cl:list)) (thing fset:map) &key &allow-other-keys)
  (fset:convert 'cl:list thing))

(defmethod as ((class (eql 'fset:map)) (thing fset:map) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'fset:map)) (thing cl:list) &key &allow-other-keys)
  (fset:convert 'fset:map thing))

;;; =====================================================================
;;; MAP API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; associate
;;; ---------------------------------------------------------------------

(defmethod associate ((m cl:list) key val)
  (acons key val m))

(defmethod associate ((m fset:map) key val)
  (fset:with m key val))

;;; ---------------------------------------------------------------------
;;; contains-key?
;;; ---------------------------------------------------------------------

(defmethod contains-key? ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (and (assoc key m :test test)
       t))

(defmethod contains-key? ((m fset:map) key &key &allow-other-keys)
  (fset:domain-contains? m key))

;;; ---------------------------------------------------------------------
;;; dissociate
;;; ---------------------------------------------------------------------

(defmethod dissociate ((m cl:list) key &key (test 'eql) &allow-other-keys)
  (remove-if (lambda (entry)
               (funcall test key (car entry))) 
             m))

(defmethod dissociate ((m fset:map) key &key &allow-other-keys)
  (fset:less m key))

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

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(defmethod keys ((m cl:list))(seq:image 'car m))
(defmethod keys ((m fset:map))(fset:domain m))

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
    ((fset:map) (as 'fset:map (apply 'make args)))
    (t (error "Unrecognized map type: ~S" type))))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(defmethod merge ((m1 cl:list) (m2 cl:list) &key (test 'eql) &allow-other-keys)
  (if (null m2)
      m1
      (let ((pair (car m2)))
        (merge (acons (car pair) (cdr pair)
                      (remove-if (lambda (u) (funcall test (car u) (car pair)))
                                 m1))
               (cdr m2)))))

(defmethod merge ((m1 fset:map) (m2 fset:map) &key &allow-other-keys)
  (fset:map-union m1 m2))

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(defmethod vals ((m cl:list))(seq:image 'cdr m))
(defmethod vals ((m fset:map))(fset:range m))

;;; ---------------------------------------------------------------------
;;; zipmap
;;; ---------------------------------------------------------------------

(defmethod zipmap ((ks cl:list)(vs cl:list))
  (seq:image (lambda (k v) (cons k v)) ks vs))
