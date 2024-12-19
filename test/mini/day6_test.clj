(ns mini.day6-test
  (:require
   [clojure.test :refer :all]
   [mini.day6 :refer :all]))

(deftest move-up-test
  (is (= (move-up [2 3]) [2 2]))
  (is (= (move-up [2 153]) [2 152]))
  (is (= (move-up [2 1]) [2 0]))
  (is (= (move-up [2 0]) [2 nil])))

(deftest move-left-test
  (is (= (move-left [2 3]) [1 3]))
  (is (= (move-left [153 3]) [152 3]))
  (is (= (move-left [1 3]) [0 3]))
  (is (= (move-left [0 3]) [nil 3])))

(deftest move-right-test
  (let [coords [[] [] [] []]]
    (is (= (move-right coords [1 3]) [2 3]))
    (is (= (move-right coords [2 3]) [3 3]))
    (is (= (move-right coords [3 3]) [nil 3]))))

(deftest move-down-test
  (let [coords [[] [] [] []]]
    (is (= (move-down coords [1 0]) [1 1]))
    (is (= (move-down coords [1 1]) [1 2]))
    (is (= (move-down coords [1 2]) [1 3]))
    (is (= (move-down coords [1 3]) [1 nil]))))


(deftest move-left-until-test
  (let [coords [[1] [1] [1] [1]]]
    (is (= (move-left-until coords [103 3]) [2 3]))
    (is (= (move-left-until coords [1 3]) [nil 3]))))