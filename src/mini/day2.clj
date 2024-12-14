(ns mini.day2
  (:require clojure.string))

(defn load-data []
  (->> (slurp "src/mini/day2.txt")
       clojure.string/split-lines
       (map #(mapv read-string (clojure.string/split % #"\s+")))
       vec))

(defn is-all-increasing [list]
  (apply <= list))

(defn is-all-decreasing [list]
  (apply >= list))

(defn offsets [list]
  (mapv #(Math/abs (apply - %)) (partition 2 1 list)))

(defn min-offset [list]
  (apply min (offsets list)))

(defn max-offset [list]
  (apply max (offsets list)))

(defn is-safe [list]
  (and (or (is-all-increasing list)
           (is-all-decreasing list))
       (>= (min-offset list) 1)
       (<= (max-offset list) 3)))

(defn are-safe [list]
  (mapv is-safe list))

(defn remove-item-at [list index]
  (concat (take index list) (drop (inc index) list)))

(defn is-safe-removing [list index]
  (is-safe (remove-item-at list index)))

(defn is-safe-or-removable [list]
  (or (is-safe list)
      (some #(is-safe-removing list %) (range (count list)))))

(defn are-safe-or-removable [list]
  (mapv is-safe-or-removable list))

(comment
  ;; part 1
  (def data
    (load-data))

  (count (filter true? (are-safe data)))
  :rcf)

(comment
    ;; part 2
  (def data
    (load-data))

  (count (filter true? (are-safe-or-removable data)))
  
  :rcf)