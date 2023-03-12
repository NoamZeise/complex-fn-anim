(defpackage cl-images/tests/main
  (:use :cl
        :cl-images
        :rove))
(in-package :cl-images/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-images)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
