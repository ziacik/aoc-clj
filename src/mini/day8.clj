(ns mini.day8
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

(defn get-antinodes-one [[[antenna1-row antenna1-col] [antenna2-row antenna2-col]]]
  (let [row-diff  (- antenna2-row antenna1-row)
        col-diff (- antenna2-col antenna1-col)]
    [[(- antenna1-row row-diff) (- antenna1-col col-diff)] [(+ antenna2-row row-diff) (+ antenna2-col col-diff)]]))

(defn get-antinodes [antennas]
  (distinct (mapcat get-antinodes-one (combinations antennas))))

(defn get-antinodes-for-freq [lines frequency]
  (get-antinodes (get-antennas lines frequency)))

(defn get-antinodes-for-all-freqs [lines]
  (distinct (mapcat (fn [frequency] (get-antinodes-for-freq lines frequency)) (get-frequencies lines))))

(defn remove-out-of-bounds [lines nodes]
  (let [line-count (count lines)]
    (filter (fn [[row col]]
              (and (>= row 0) (< row line-count)
                   (>= col 0) (< col line-count)))
            nodes)))

(comment
  ;; part 1

  (def lines (load-data))

  (count (remove-out-of-bounds lines (get-antinodes-for-all-freqs lines)))

  :rcf)


