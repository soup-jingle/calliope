(defpackage calliope/tests/main
  (:use :cl
        :calliope
        :rove))
(in-package :calliope/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :calliope)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
