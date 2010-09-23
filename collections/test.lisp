;;; ---------------------------------------------------------------------
;;; COLLETIONS test suite
;;; ---------------------------------------------------------------------

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.TEST"
  (:use :cl :as :fn :lift))

(in-package :folio.collections.test)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite collections-tests () ())

;;; ---------------------------------------------------------------------
;;;  set AS tests
;;; ---------------------------------------------------------------------

(deftestsuite set-as-tests (collections-tests) ())

(addtest (set-as-tests)
  test-fset-set-to-fset-set
  (let ((s (set:make 0 1 2 3)))
    (ensure-same :equal
                 (fset:compare s (as 'fset:set s)))))

(addtest (set-as-tests)
  test-fset-seq-to-fset-set
  (let* ((s1 (seq:make 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-fset-seq
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'fset:seq s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-list-to-fset-set
  (let* ((s1 (list 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-list
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'list s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-vector-to-fset-set
  (let* ((s1 (vector 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-vector
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'vector s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-string-to-fset-set
  (let* ((s1 "A test string")
         (s2 (as 'fset:set s1)))
    (ensure
     (every 'identity 
            (as 'list
                (seq:image (fn (u v) (char= u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-string
  (let* ((s1 "AbCd")
         (s2 (set:make #\A #\b #\C #\d)))
    (ensure
     (every (fn (ch) (member ch (as 'list s1) :test 'char=)) 
            (as 'list (as 'string s2))))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'set-as-tests)

;;; ---------------------------------------------------------------------
;;;  set API tests
;;; ---------------------------------------------------------------------

(deftestsuite set-api-tests (collections-tests) ())

(addtest (set-api-tests)
  test-adjoin
  (ensure (set:contains? (set:adjoin 12345 '(0 1 2 3))
                     12345))
  (ensure (set:contains? (set:adjoin 12345 (set:make 0 1 2 3))
                     12345)))


;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'set-api-tests)

