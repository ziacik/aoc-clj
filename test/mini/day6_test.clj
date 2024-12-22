(ns mini.day6-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require
   [clojure.test :refer :all]
   [mini.day6 :refer :all]))

(deftest parse-board-test
  (is (= [[0 2]
          []
          [1 2]]
         (parse-board ["#.#"
                       ".^."
                       ".##"]))))

(deftest move-up-test
  (is (= [2 2] (move-up [2 3])))
  (is (= [2 152] (move-up [2 153])))
  (is (= [2 0] (move-up [2 1])))
  (is (= [2 -1] (move-up [2 0]))))

(deftest move-left-test
  (is (= [1 3] (move-left [2 3])))
  (is (= [152 3] (move-left [153 3])))
  (is (= [0 3] (move-left [1 3])))
  (is (= [-1 3] (move-left [0 3]))))

(deftest move-right-test
  (is (= [2 3] (move-right [1 3])))
  (is (= [3 3] (move-right [2 3])))
  (is (= [4 3] (move-right [3 3]))))

(deftest move-down-test
  (is (= [1 1] (move-down [1 0])))
  (is (= [1 2] (move-down [1 1])))
  (is (= [1 3] (move-down [1 2])))
  (is (= [1 4] (move-down [1 3]))))

(deftest out?-test
  (let [board [[1] [1] [1] [1]]]
    (is (out? board [-1 0]))
    (is (out? board [4 3]))
    (is (out? board [3 -1]))
    (is (out? board [3 4]))
    (is (not (out? board [1 0])))
    (is (not (out? board [1 3])))
    (is (not (out? board [3 1])))
    (is (not (out? board [3 2])))))

(deftest move-left-until-test
  (let [board [[1] [1] [1] [1]]]
    (is (= [2 3] (move-left-until board [3 3])))
    (is (= [-1 3] (move-left-until board [1 3])))))

(deftest move-right-until-test
  (let [board [[1] [1] [1] [1]]]
    (is (= [4 3] (move-right-until board [2 3])))
    (is (= [0 3] (move-right-until board [0 3])))))

(deftest move-up-until-test
  (let [board [[] [0 1 2 3] [] []]]
    (is (= [3 2] (move-up-until board [3 3])))
    (is (= [1 -1] (move-up-until board [1 1])))))

(deftest move-down-until-test
  (let [board [[]
               []
               [0 1 2 3]
               []]]
    (is (= [2 1] (move-down-until board [2 0])))
    (is (= [2 4] (move-down-until board [2 3])))))

(deftest get-starting-position-test
  (let [lines ["#.#"
               ".^."
               "#.#"]]
    (is (= [1 1] (get-starting-position lines)))))

(deftest parse-board-test
  (let [lines ["#.#"
               ".^."
               ".##"]]
    (is (= [[0 2]
            []
            [1 2]]
           (parse-board lines)))))

(deftest obstacle?-test
  (let [board [[0 2]
               []
               [1 2]]]
    (is (obstacle? board [0 0]))
    (is (obstacle? board [2 0]))
    (is (obstacle? board [1 2]))
    (is (obstacle? board [2 2]))
    (is (not (obstacle? board [1 0])))
    (is (not (obstacle? board [0 1])))
    (is (not (obstacle? board [1 1])))
    (is (not (obstacle? board [2 1])))
    (is (not (obstacle? board [0 2])))))

(deftest all-positions-between-test
  (let [board [[0 2]
               []
               [1 2]]]
    (is (= [[0 2] [1 2]] (all-positions-between board [0 2] [1 2])))
    (is (= [[0 0] [0 1] [0 2]] (all-positions-between board [0 2] [0 0])))
    (is (= [[0 2] [1 2] [2 2]] (all-positions-between board [0 2] [2 2])))
    (is (= [[0 0] [0 1] [0 2]] (all-positions-between board [0 2] [0 -1])))))

(deftest can-find-path-test
  (let [board [[0 2]
               []
               [1 2]]]
    (is (= [[0 2] [0 1] [1 1] [2 1]] (find-path-unique board [0 2])))))

(deftest find-path-avoids-infinite-loop-test
  (let [board [[1]
               [2]
               [0]
               [1]]]
    (is (= nil (find-path board [1 2])))))

(deftest find-path-avoids-infinite-loop-test-negative
  (let [board [[1]
               [2]
               [0]
               []]]
    (is (not= nil (find-path board [1 2])))))

(deftest make-all-possible-boards-with-one-coord-added-test
  (let [board [[0 2]
               []
               [1 2]]
        except [1 1]]
    (is (= [[[0 1 2] [] [1 2]]
            [[0  2] [0] [1 2]]
            [[0  2] [2] [1 2]]
            [[0  2] [] [0 1 2]]]
           (make-all-possible-boards-with-one-coord-added board except)))))

(deftest find-infinite-loops-test
  (let [board [[1]
               [2]
               [0]
               []]]
    (is (= [[[1]
             [2]
             [0]
             [1]]] (find-infinite-loops board [1 1])))))