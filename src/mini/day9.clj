(ns mini.day9)

(defn load-data []
  (seq (slurp "src/mini/day9.txt")))

(defn decode [data]
  (loop [chars data
         result []
         file-id 0]
    (if (empty? chars)
      result
      (let [file-len (- (int (first chars)) (int \0))
            free-len (if (> (count chars) 1)
                       (- (int (second chars)) (int \0))
                       0)
            file-chars (repeat file-len file-id)
            free-chars (repeat free-len nil)]
        (recur (drop 2 chars)
               (into result (concat file-chars free-chars))
               (inc file-id))))))

(defn replace-first-nil [s]
  (let [first-nil-idx (->> s
                           (map-indexed vector)
                           (filter (comp nil? second))
                           first
                           first)
        last-non-nil-idx (->> s
                              (map-indexed vector)
                              (filter (comp some? second))
                              last
                              first)]
    (if (>= first-nil-idx last-non-nil-idx)
      s
      (-> s
          (assoc first-nil-idx (nth s last-non-nil-idx))
          (assoc last-non-nil-idx nil)))))

(defn replace-nils [s]
  (println "count:" (count s))
  (reduce (fn [seq idx]
            (when (zero? (mod idx 1000))
              (println "Progress:" idx "/" (count s)))
            (replace-first-nil seq))
          s
          (range (count s))))


(defn checksum [s]
  (reduce + (map-indexed (fn [idx val] (* idx val)) (remove nil? s))))

(comment
  ;; Part 1
  (-> (load-data)
      decode
      replace-nils
      checksum)

  :rfc)
  