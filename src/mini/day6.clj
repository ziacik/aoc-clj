(ns mini.day6
  (:require
   [clojure.string :as string]))

(defn load-data []
  (->> (slurp "src/mini/day6.txt")
       string/split-lines))

(defn parse-board [lines]
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


(defn out? [board [x y]]
  (or (< x 0)
      (< y 0)
      (>= x (count board))
      (>= y (count board))))

(defn in? [board [x y]]
  (not (out? board [x y])))

(defn obstacle? [board [x y]]
  (and (in? board [x y])
       (some #{x} (nth board y))))

(defn move-left [[x y]]
  [(- x 1) y])

(defn move-up [[x y]]
  [x (- y 1)])

(defn move-right [[x y]]
  [(+ x 1) y])

(defn move-down [[x y]]
  [x (+ y 1)])

(defn move-left-until [board pos]
  (if (and (in? board pos) (not (obstacle? board (move-left pos))))
    (recur board (move-left pos))
    pos))

(defn move-up-until [board pos]
  (if (and (in? board pos) (not (obstacle? board (move-up pos))))
    (recur board (move-up pos))
    pos))

(defn move-right-until [board pos]
  (if (and (in? board pos) (not (obstacle? board (move-right pos))))
    (recur board (move-right pos))
    pos))

(defn move-down-until [board pos]
  (if (and (in? board pos) (not (obstacle? board (move-down pos))))
    (recur board (move-down pos))
    pos))

(defn into-board [board [x y]]
  (let [max-idx (dec (count board))
        x' (cond
             (< x 0) 0
             (> x max-idx) max-idx
             :else x)
        y' (cond
             (< y 0) 0
             (> y max-idx) max-idx
             :else y)]
    [x' y']))

(defn all-positions-between [board positionA positionB]
  (let [[x1 y1] (into-board board positionA)
        [x2 y2] (into-board board positionB)]
    (cond
      (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))]
                  [x1 y])
      (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))]
                  [x y1])
      :else (throw (ex-info "Not a straight line" {:positionA positionA :positionB positionB})))))

(defn find-path [board starting-position]
  (let [directions [move-up-until move-right-until move-down-until move-left-until]]
    (loop [position starting-position
           repeated-directions (cycle directions)
           path [starting-position]
           visited #{}]
      (let [dir (first repeated-directions)
            new-position (dir board position)
            positions-between (all-positions-between board position new-position)
            visited-key [new-position dir]]
        (cond
          (contains? visited visited-key)
          nil
          
          (out? board new-position)
          (into path positions-between)

          :else
          (recur new-position
                 (rest repeated-directions)
                 (into path positions-between)
                 (conj visited visited-key)))))))

(defn make-all-possible-boards-with-one-coord-added [board except-coord]
  (for [y (range (count board))
        x (range (count board))
        :let [new-board (vec (map vec board))]
        :when (and (not (obstacle? new-board [x y]))
                   (not= [x y] except-coord))]
    (assoc new-board
           y
           (-> (conj (new-board y) x)
               sort
               vec))))

(defn find-infinite-loops [board starting-position]
  (let [boards (make-all-possible-boards-with-one-coord-added board starting-position)]
    (filter (fn [board]
              (nil? (find-path board starting-position)))
            boards)))

(defn find-path-unique [board starting-position]
  (distinct (find-path board starting-position)))

(comment
  (def board (parse-board (load-data)))
  (def starting-coord (get-starting-position (load-data)))

  (count (find-path-unique board starting-coord))

  :rcf)

(comment
  (def board (parse-board (load-data)))
  (def starting-coord (get-starting-position (load-data)))

  (count (find-infinite-loops board starting-coord))
  :rcf)
  
