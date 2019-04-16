(fiasco:define-test-package :brain-tests
    (:use :brain :alexandria))
(in-package :brain-tests)

(defparameter grid-1 (alist-hash-table '(((3 . 3) . :on)) :test #'equal))

(defparameter grid-2 (alist-hash-table '(((1 . 0) . :on)
					 ((1 . 2) . :on)
					 ((0 . 1) . :on)
					 ((2 . 1) . :on)) :test #'equal))

(deftest new-grid-test ()
  (is (eq 'hash-table (type-of (new-grid 2 2)))))

(deftest count-live-neighbours-test ()
  (is (= 1 (count-live-neighbours grid-1 3 2)))
  (is (= 1 (count-live-neighbours grid-1 3 4)))
  (is (= 1 (count-live-neighbours grid-1 4 3)))
  (is (= 1 (count-live-neighbours grid-1 2 3)))
  (is (= 4 (count-live-neighbours grid-2 1 1))))

(deftest grid->list-test ()
  (is (equal '((:tag . :cells) (:cells ((:color . 1) (:point 3 3)))) (grid->list grid-1))))

(deftest next-grid-test ()
  (let* ((g (copy-hash-table grid-1 :test #'equal))
	 (r (hash-table-alist (next-grid g 5 5))))
    (is (equal r '(((3 . 3) . :dying))))))
