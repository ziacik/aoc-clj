(ns mini.day8.2
  (:require
   [clojure.string :as string]))

(defn load-data []
  (-> (slurp "src/mini/day8.txt")
      (string/split-lines)))

(defn get-frequencies [lines]
  (set (filter #(not= % \.) (apply concat lines))))

(defn get-antennas [lines frequency]
  (for [row (range (count lines))
        col (range (count (nth lines row)))
        :when (= (get-in lines [row col]) frequency)]
    [row col]))

(defn combinations [coll]
  (mapcat (fn [x] (map #(vector x %) (rest (drop-while #(not= % x) coll)))) coll))

(defn in-bounds? [lines [r c]]
  (let [lc (count lines)]
    (and (<= 0 r) (< r lc)
         (<= 0 c) (< c lc))))

(defn finite-antinodes
  [lines [[r1 c1] [r2 c2]]]
  (let [dr (- r2 r1)
        dc (- c2 c1)
        in-bounds? (partial in-bounds? lines)]
    (concat
         ;; Backwards from antenna1
     (take-while in-bounds?
                 (iterate (fn [[r c]]
                            [(- r dr) (- c dc)])
                          [r1 c1]))
         ;; Forwards from antenna2
      (take-while in-bounds?
                  (iterate (fn [[r c]]
                             [(+ r dr) (+ c dc)])
                           [r2 c2])))))

(defn get-antinodes [lines antennas]
  (distinct (mapcat finite-antinodes lines (combinations antennas))))

(defn get-antinodes-for-freq [lines frequency]
  (get-antinodes lines (get-antennas lines frequency)))

(defn get-antinodes-for-all-freqs [lines]
  (distinct (mapcat (fn [frequency] (get-antinodes-for-freq lines frequency)) (get-frequencies lines))))


(comment
  ;; part 2

  (def lines (load-data))

  (count (get-antinodes-for-all-freqs lines))
  
  :rcf)
