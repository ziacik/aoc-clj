(ns mini.day1
  (:require clojure.string))

(defn load-data []
  (->> (slurp "src/mini/day1.txt")
       clojure.string/split-lines
       (map #(mapv read-string (clojure.string/split % #"\s+")))
       vec))

(defn first-column [list] (mapv first list))
(defn second-column [list] (mapv second list))

(defn first-sorted [list] (sort (first-column list)))
(defn second-sorted [list] (sort (second-column list)))

(defn distances [list1 list2]
    (mapv #(Math/abs (- %1 %2)) list1 list2))

(defn sum [list] (reduce + list))

(defn frequencies-second [list]
  (frequencies (second-column list)))

(defn frequencies-of-first-in-second [list]
  (map (fn [x] (get (frequencies-second list) x 0))
       (first-column list)))

(defn scores [list]
  (map(fn [x y] (* x y))
       (frequencies-of-first-in-second list)
       (first-column list)))

(comment
  ;; part 1
  (def data
    (load-data))

  (sum (distances (first-sorted data) (second-sorted data)))
  :rcf)

(comment
  ;; part 2
  (def data
    (load-data))

  (sum (scores data))
  :rcf)