(ns mini.day7-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require
   [clojure.test :refer :all]
   [mini.day7 :refer :all]))

(deftest parse-lines-test
  (let [lines ["190: 10 19"
               "3267: 81 40 27"]]
    (is (= (parse-lines lines) [[190 [10 19]] [3267 [81 40 27]]]))))

(deftest operate-test
  (is (= [29 190] (operate [10 19] :default)))
  (is (= [6 9 5 6] (operate [1 2 3] :default))))
