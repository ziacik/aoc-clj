(ns mini.day6
  (:require
   [clojure.string :as string]))

(defn load-data []
  (->> (slurp "src/mini/day6.txt")
       string/split-lines))

(defn parse-coords [lines]
  (map (fn [line]
         (keep-indexed (fn [x char]
                         (when (= char \#)
                           x))
                       line))
       lines))

(defn get-starting-position [lines]
  (first (for [[y line] (map-indexed vector lines)
               :let [x (string/index-of line "^")]
               :when (not (nil? x))]
           [x y])))

(defn- has-position [coords [x y]]
  (and (some? x)
       (some? y)
       (some #{x} (nth coords y))))

(defn move-left [[x y]]
  (if (> x 0)
    [(- x 1) y]
    [nil y]))

(defn move-up [[x y]]
  (if (> y 0)
    [x (- y 1)]
    [x nil]))

(defn move-right [coords, [x y]]
  (if (< x  (dec (count coords)))
    [(+ x 1) y]
    [nil y]))

(defn move-down [coords, [x y]]
  (if (< y (dec (count coords)))
    [x (+ y 1)]
    [x nil]))

(defn move-left-until [coords [x y]]
  (if (and (some? x) (not (has-position coords (move-left [x y]))))
    (recur coords (move-left [x y]))
    [x y]))

(defn move-up-until [coords [x y]]
  (if (and (some? y) (not (has-position coords (move-up [x y]))))
    (recur coords (move-up [x y]))
    [x y]))


(defn move-right-until [coords [x y]]
  (if (and (some? x) (not (has-position coords (move-right coords [x y]))))
    (recur coords (move-right coords [x y]))
    [x y]))


(defn move-down-until [coords [x y]]
  (if (and (some? y) (not (has-position coords (move-down coords [x y]))))
    (recur coords (move-down coords [x y]))
    [x y]))

(defn all-positions-between [coords positionA positionB]
  (let [x1 (first positionA)
        y1 (second positionA)
        x2 (if (nil? (first positionB)) (dec (count coords)) (first positionB))
        y2 (if (nil? (second positionB)) (dec (count coords)) (second positionB))]
    (cond
      (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))]
                  [x1 y])
      (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))]
                  [x y1])
      :else (throw (ex-info "Not a straight line" {:positionA positionA :positionB positionB})))))

(defn find-path [coords starting-position]
  (let [directions [move-up-until move-right-until move-down-until move-left-until]]
    (loop [position starting-position
           repeated-directions (cycle directions)
           path [starting-position]]
      (let [new-position ((first repeated-directions) coords position)]
        (println "From " position " to " new-position)
        (let [positions-between (all-positions-between coords position new-position)]
          (if (or (nil? (first new-position))
                  (nil? (second new-position)))
            (into path positions-between)
            (recur new-position (rest repeated-directions) (into path positions-between))))))))

(defn find-path-unique [coords starting-position]
  (distinct (find-path coords starting-position)))

(comment
  (def coords (parse-coords (load-data)))
  (def starting-coords (get-starting-position (load-data)))

  (count (distinct (all-positions-between coords [0 0] [1 0])))
  (count (find-path-unique coords starting-coords))

  :rcf)
