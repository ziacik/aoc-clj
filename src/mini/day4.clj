(ns mini.day4
  (:require
   [clojure.string :as string]))

(defn load-data []
  (->> (slurp "src/mini/day4.txt")
       string/split-lines
       (mapv vec)))

(defn find-xmas [grid]
  (let [rows (count grid)
        cols (count (first grid))
        word (seq "XMAS")
        directions [[-1 -1] [-1 0] [-1 1]
                    [0 -1]        [0 1]
                    [1 -1] [1 0] [1 1]]]
    (for [i (range rows)
          j (range cols)
          [dx dy] directions
          :let [positions (map (fn [k]
                                 [(+ i (* k dx))
                                  (+ j (* k dy))])
                               (range (count word)))
                letters (map (fn [[x y]]
                               (when (and (<= 0 x (dec rows))
                                          (<= 0 y (dec cols)))
                                 (nth (nth grid x) y)))
                             positions)]
          :when (= letters word)]
      positions)))

(defn find-mas [grid]
  (let [rows (count grid)
        cols (count (first grid))
        word (seq "MAS")
        directions [[-1 -1]  [-1 1]
                    [1 -1]  [1 1]]]
    (for [i (range rows)
          j (range cols)
          [dx dy] directions
          :let [positions (map (fn [k]
                                 [(+ i (* k dx))
                                  (+ j (* k dy))])
                               (range (count word)))
                letters (map (fn [[x y]]
                               (when (and (<= 0 x (dec rows))
                                          (<= 0 y (dec cols)))
                                 (nth (nth grid x) y)))
                             positions)]
          :when (= letters word)]
      positions)))

(defn how-many-have-the-same-second-coord [positions]
  (->> positions
       (map second)
       frequencies
       (filter #(= 2 (val %)))
       count))


(comment
  ;; part 1
  (def data
    (load-data))

  (count (find-xmas data))
  :rcf)

(comment
    ;; part 2
  (def data
    (load-data))

  (how-many-have-the-same-second-coord (find-mas data))

  :rcf)
     
     
