
(in-package :mizar)

(5am:def-suite author-suite
    :description "A 5am test suite for testing functionality for the mizar author class.")

(5am:in-suite author-suite)

(5am:test missing-name-initarg-error
  (5am:signals nameless-author-error
    (make-instance 'author)))

(5am:test (non-numeric-author-error :compile-at :run-time)
  (5am:signals nameless-author-error
    (make-instance 'author :name 5)))

(5am:test (identical-objects :compile-at :run-time)
  (5am:is (eq (make-instance 'author :name "Steve")
	      (make-instance 'author :name "Steve"))))
