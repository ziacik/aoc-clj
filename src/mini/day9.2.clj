(ns mini.day9.2)

(defn load-data []
  (slurp "src/mini/day9.txt"))

(defn decode-disk-map [disk-map]
  (loop [segments []
         ids 0
         input (map #(Character/digit % 10) disk-map)]
    (if (empty? input)
      segments
      (let [[file-len free-len & rest] input]
        (recur (conj segments {:type :file :length file-len :id ids}
                     {:type :free :length (or free-len 0)})
               (inc ids)
               rest)))))

(defn find-free-space [segments from-index size]
  (loop [curidx 0]
    (if (>= curidx from-index)
      nil
      (let [segment (nth segments curidx)]
        (if (and (= :free (:type segment)) (>= (:length segment) size))
          {:segment segment :index curidx}
          (recur (inc curidx)))))))

(defn replace-at [vector index new-items]
  (concat (take index vector) new-items (drop (inc index) vector)))

(defn merge-free-space [segments]
  (time (reduce (fn [result segment]
                  (if (empty? result)
                    [segment]
                    (let [last (last result)]
                      (if (and (= :free (:type last)) (= :free (:type segment)))
                        (conj (butlast result) {:type :free :length (+ (:length last) (:length segment))})
                        (conj result segment))))) [] segments)))

(defn find-file-by-id [segments id]
  (some (fn [[index segment]]
          (when (= (:id segment) id)
            {:segment segment
             :index index}))
        (map-indexed vector segments)))

(defn move-file [segments id]
  (let [file (find-file-by-id segments id)
        file-index (:index file)
        file-segment (:segment file)
        file-size (:length file-segment)
        free-space (find-free-space segments file-index file-size)]
    (if (some? free-space)
      (let [free-space-index (:index free-space)
            free-space-segment (:segment free-space)
            free-space-size (:length free-space-segment)
            new-free-space-size (- free-space-size file-size)
            new-free-segment {:type :free :length new-free-space-size}
            new-free-segment-of-file {:type :free :length file-size}
            new-segments (if (zero? new-free-space-size)
                           [file-segment]
                           [file-segment new-free-segment])]
        (replace-at (replace-at segments file-index [new-free-segment-of-file]) free-space-index new-segments))
      segments)))

(defn max-id [segments]
  (apply max (map #(:id %) (filter #(= :file (:type %)) segments))))

(defn move-files [segments]
  (println "max id:" (max-id segments))
  (loop [result segments
         current-id (max-id segments)]
    (when (zero? (mod current-id 100))
      (println "Processing id:" current-id))
    (if (<= 0 current-id)
      (recur (move-file result current-id) (dec current-id))
      result)))


(defn checksum-segment [segment position]
  (if (= :file (:type segment))
    (reduce + (map #(* (:id segment) (+ position %)) (range (:length segment))))
    0))

(defn checksum [segments]
  (loop [result 0
         position 0
         rest-segments segments]
    (if (empty? rest-segments)
      result
      (let [segment (first rest-segments)]
        (recur (+ result (checksum-segment segment position))
               (+ position (:length segment))
               (rest rest-segments))))))

(def x (load-data))
(def y (decode-disk-map x))

(filter #(= -1  (:length %)) y)


(comment
  ;; Part 2

  (-> (load-data)
      decode-disk-map
      move-files
      checksum)

  :rfc)
  