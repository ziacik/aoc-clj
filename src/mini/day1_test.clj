(ns mini.day1-test
  (:require [clojure.test :refer [deftest is]]))

(deftest test-example
  (is (= 1 1))
  (is (= (+ 1 1) 2)))