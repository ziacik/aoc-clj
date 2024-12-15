(ns mini.day3)


(defn load-data []
  (->> (slurp "src/mini/day3.txt")))

(defn find-mul-instances [s]
  (re-seq #"mul\(\d+,\d+\)" s))

(defn parse-mul [s]
  (let [[_ a b] (re-matches #"mul\((\d+),(\d+)\)" s)]
    [(Integer/parseInt a) (Integer/parseInt b)]))

(defn remove-between-dont-and-do [s]
  (clojure.string/replace s #"(?s)don't\(\).*?(do\(\)|$)" ""))

(comment
    ;; part 1
  (def data
    (load-data))

  (->> data
       find-mul-instances
       (map parse-mul)
       (map (fn [[a b]] (* a b)))
       (reduce +))
  :rcf)

(comment
    ;; part 2
  (def data
    (load-data))

  (->> data
       remove-between-dont-and-do
       find-mul-instances
       (map parse-mul)
       (map (fn [[a b]] (* a b)))
       (reduce +))
  :rcf)

