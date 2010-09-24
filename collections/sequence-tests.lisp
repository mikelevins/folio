;;; ---------------------------------------------------------------------
;;; SEQUENCES test suite
;;; ---------------------------------------------------------------------

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.SEQUENCES.TEST"
  (:use :cl :as :fn :lift))

(in-package :folio.collections.sequences.test)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite sequence-tests () ())

;;; ---------------------------------------------------------------------
;;;  sequence AS tests
;;; ---------------------------------------------------------------------

(deftestsuite sequence-as-tests (sequence-tests) ())

(addtest (sequence-as-tests)
  test-fset-seq-to-vector
  (let ((s (seq:make-as 'fset:seq 0 1 2 3)))
    (ensure (vectorp (as 'vector s)))
    (ensure (every (fn (e)(fset:find e s))
                   (as 'vector s)))))

(addtest (sequence-as-tests)
  test-vector-to-fset-seq
  (let* ((v (vector 0 1 2 3))
         (s (as 'fset:seq v)))
    (ensure (every (fn (e)(fset:find e s))
                   v))))

(addtest (sequence-as-tests)
  test-fset-seq-to-list
  (let ((s (seq:make-as 'fset:seq 0 1 2 3)))
    (ensure (listp (as 'list s)))
    (ensure (every (fn (e)(fset:find e s))
                   (as 'list s)))))

(addtest (sequence-as-tests)
  test-list-to-fset-seq
  (let* ((l (list 0 1 2 3))
         (s (as 'fset:seq l)))
    (ensure (every (fn (e)(fset:find e s))
                   l))))


(addtest (sequence-as-tests)
  test-string-to-fset-seq
  (let* ((s "A test string")
         (f (as 'fset:seq s)))
    (ensure (every (fn (e)(fset:find e s))
                   (as 'list f)))))

(addtest (sequence-as-tests)
  test-fset-seq-to-string
  (let* ((f (seq:make-as 'fset:seq #\F #\o #\o))
         (s "Foo"))
    (ensure (string= s (as 'string f)))))

(addtest (sequence-as-tests)
  test-fset-seq-to-fset-seq
  (let ((s (seq:make-as 'fset:seq 0 1 2 3)))
    (ensure-same :equal
                 (fset:compare s (as 'fset:seq s)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'sequence-as-tests)

;;; ---------------------------------------------------------------------
;;;  sequence API tests
;;; ---------------------------------------------------------------------

(deftestsuite sequence-api-tests (sequence-tests) ())

(addtest (sequence-api-tests)
  test-concat
  (let ((l1 (list 0 1 2))
        (l2 (list 3 4 5))
        (lexpected (list 0 1 2 3 4 5))
        (v1 (vector 0 1 2))
        (v2 (vector 3 4 5))
        (vexpected (vector 0 1 2 3 4 5))
        (f1 (fset:seq 0 1 2))
        (f2 (fset:seq 3 4 5))
        (fexpected (fset:seq 0 1 2 3 4 5)))
    (ensure (equal lexpected (seq:concat l1 l2)))
    (ensure (equalp vexpected (seq:concat v1 v2)))
    (ensure (eql :equal (fset:compare fexpected (seq:concat f1 f2))))
    (ensure (listp (seq:concat l1 v1)))
    (ensure (typep (seq:concat f1 v1) 'fset:seq))))

(addtest (sequence-api-tests)
  test-contains?
  (ensure (seq:contains? (list 0 "foo" 123 10) 123))
  (ensure (seq:contains? (fset:seq 0 "foo" 123 10) 123)))

(addtest (sequence-api-tests)
  test-difference
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 3 4 5))
        (expected (list 0 1 2)))
    (ensure (fset:empty? (seq:difference (seq:difference (as 'fset:seq s1)
                                                         (as 'fset:seq s2))
                                         (as 'fset:seq expected))))
    (ensure (fset:empty? (seq:difference (as 'fset:seq expected)
                                         (seq:difference (as 'fset:seq s1)
                                                         (as 'fset:seq s2)))))
    (ensure (null (seq:difference expected (seq:difference s1 s2))))))

(addtest (sequence-api-tests)
  test-drop
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 0 1 2))
        (s3 (list 3 4 5)))
    (ensure (equal s3 (seq:drop 3 s1)))
    (ensure (equal s2 (seq:drop 3 s1 :from-end? t)))))

(addtest (sequence-api-tests)
  test-drop-while
  (let ((s1 (list 1 1 1 2 2 2))
        (s2 (list 1 1 1))
        (s3 (list 2 2 2)))
    (ensure (equal s3 (seq:drop-while 'oddp s1)))
    (ensure (eql :equal (fset:compare (as 'fset:seq s3) (seq:drop-while 'oddp (as 'fset:seq s1)))))
    (ensure (equal s2(seq:drop-while 'evenp s1 :from-end? t)))))

(addtest (sequence-api-tests)
  test-element
  (let ((l (list 0 1 2 3 4 5))
        (v (vector 0 1 2 3 4 5))
        (s "012345")
        (f (fset:seq 0 1 2 3 4 5)))
    (ensure (eql 3 (seq:element l 3)))
    (ensure (eql 3 (seq:element v 3)))
    (ensure (char= #\3 (seq:element s 3)))
    (ensure (eql 3 (seq:element f 3)))))

(addtest (sequence-api-tests)
  test-empty?
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (seq:empty? nil))
    (ensure (seq:empty? (fset:seq)))
    (ensure (not (seq:empty? (as 'fset:seq l))))))

(addtest (sequence-api-tests)
  test-every
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (seq:every? 'integerp l))
    (ensure (not (seq:every? 'oddp l)))
    (ensure (seq:every? 'integerp (as 'fset:seq l)))
    (ensure (not (seq:every? 'oddp (as 'fset:seq l))))))

(addtest (sequence-api-tests)
  test-filter
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (seq:empty? (seq:filter 'stringp l)))
    (ensure (seq:empty? (seq:filter 'stringp (as 'fset:seq l))))
    (ensure (not (seq:empty? (seq:filter 'oddp l))))
    (ensure (not (seq:empty? (seq:filter 'oddp (as 'fset:seq l)))))))

(addtest (sequence-api-tests)
  test-find
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (seq:find 'oddp l))
    (ensure (not (seq:find 'stringp l)))
    (ensure (seq:find 'oddp (as 'fset:seq l)))
    (ensure (not (seq:find 'stringp (as 'fset:seq l) :from-end? t)))))

(addtest (sequence-api-tests)
  test-head
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (zerop (seq:head l)))
    (ensure (zerop (seq:head (as 'vector l))))
    (ensure (zerop (seq:head (as 'fset:seq l))))))

(addtest (sequence-api-tests)
  test-image
  (let ((l (list 0 1 2 3 4 5)))
    (ensure (seq:every? 'floatp (seq:image 'float l)))
    (ensure (seq:every? 'floatp (seq:image 'float (as 'vector l))))
    (ensure (seq:every? 'floatp (seq:image 'float (as 'fset:seq l))))))

(addtest (sequence-api-tests)
  test-interleave
  (let ((l (list 0 2 4))
        (v (list 1 3 5)))
    (ensure (oddp (seq:element (seq:interleave l v) 3)))
    (ensure (evenp (seq:element (seq:interleave (as 'fset:seq l) v) 4)))))

(addtest (sequence-api-tests)
  test-interpose
  (let ((l (list 0 2 4))
        (s ","))
    (ensure (evenp (seq:element (seq:interpose s l) 2)))
    (ensure (stringp (seq:element (seq:interpose s l) 3)))))

(addtest (sequence-api-tests)
  test-intersection
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 3 4 5 6 7 8))
        (expected (list 3 4 5)))
    (ensure (seq:empty? (seq:difference  (as 'fset:seq expected)
                                         (seq:intersection (as 'fset:seq s1)
                                                           (as 'fset:seq s2)))))
    (ensure (seq:empty? (seq:difference  (seq:intersection (as 'fset:seq s1)
                                                           (as 'fset:seq s2))
                                         (as 'fset:seq expected))))
    (ensure (null (seq:difference expected (seq:intersection s1 s2))))
    (ensure (null (seq:difference (seq:intersection s1 s2) expected)))))

(addtest (sequence-api-tests)
  test-length
  (let ((s0 (list))
        (s1 (list 1))
        (s2 (list 1 2))
        (s10 (list 1 2 3 4 5 6 7 8 9 10)))
    (ensure (zerop (seq:length s0)))
    (ensure (zerop (seq:length (as 'vector s0))))
    (ensure (zerop (seq:length (as 'fset:seq s0))))
    (ensure (= 1 (seq:length s1)))
    (ensure (= 2 (seq:length (as 'vector s2))))
    (ensure (= 10 (seq:length (as 'fset:seq s10))))))

(addtest (sequence-api-tests)
  test-make
  (let ((s0 (seq:make 0 1 2 3))
        (s1 (seq:make-as 'list 0 1 2 3))
        (s2 (seq:make-as 'vector 0 1 2 3))
        (s3 (seq:make-as 'string #\0 #\1 #\2 #\3))
        (s4 (seq:make-as 'fset:seq 0 1 2 3)))
    (ensure (seq:sequence? s0))
    (ensure (listp s1))
    (ensure (vectorp s2))
    (ensure (stringp s3))
    (ensure (typep s4 'fset:seq))))

(addtest (sequence-api-tests)
  test-partition
  (let ((s (list 1 :two 3 :four 5 :six 7 :eight)))
    (ensure (seq:every? 'numberp (seq:image 'seq:head (seq:partition 1 s :step 2))))
    (ensure (= 2 (seq:length (seq:partition 4 (as 'fset:seq s)))))))


;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'sequence-api-tests)
