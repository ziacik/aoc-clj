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



(defn- has-coord [coords [x y]]
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
  (if (and (some? x) (not (has-coord coords (move-left [x y]))))
    (recur coords (move-left [x y]))
    [x y]))

(defn move-up-until [coords [x y]]
  (if (and (some? y) (not (has-coord coords (move-up [x y]))))
    (recur coords (move-up [x y]))
    [x y]))


(defn move-right-until [coords [x y]]
  (if (and (some? x) (not (has-coord coords (move-right coords [x y]))))
    (recur coords (move-right coords [x y]))
    [x y]))


(defn move-down-until [coords [x y]]
  (if (and (some? y) (not (has-coord coords (move-down coords [x y]))))
    (recur coords (move-down coords [x y]))
    [x y]))

(defn path-cycle [coords starting-position]
  (let [directions [move-up-until move-right-until move-down-until move-left-until]]
    (loop [position starting-position
           repeated-directions (cycle directions)]
      (let [new-coords ((first repeated-directions) coords position)]
        (println "After moving:" (first repeated-directions))
        (println "Current position:" position)
        (println "New coordinates:" new-coords)
        (if (or (nil? (first new-coords))
                (nil? (second new-coords)))
          new-coords
          (recur new-coords (rest repeated-directions)))))))

(comment
  (def coords (parse-coords (load-data)))
  (def starting-coords (get-starting-position (load-data)))

  (path-cycle coords starting-coords)

  :rcf)
