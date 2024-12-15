(ns mini.day5
  (:require
   [clojure.string :as string]))

(defn load-data []
  (->> (slurp "src/mini/day5.txt")
       string/split-lines))

(defn parse-ordering-rules [lines]
  (->> lines
       (take-while (complement string/blank?))
       (map #(string/split % #"\|"))))

(defn parse-updates [lines]
  (->> lines
       (drop-while (complement string/blank?))
       (drop 1)
       (map #(string/split % #","))))

(defn conforms-to-rule [rule update]
  (let [[first-num second-num] rule
        idx-first (some #(when (= % first-num) %) update)
        idx-second (some #(when (= % second-num) %) update)]
    (or (nil? idx-first)
        (nil? idx-second)
        (< (.indexOf update first-num) (.indexOf update second-num)))))

(defn conforms-to-rules [rules update]
  (every? #(conforms-to-rule % update) rules))

(defn middle-element [coll]
  (let [n (count coll)]
    (nth coll (quot n 2))))


(defn make-conform [rules update]
  (sort (fn [a b]
          (if (conforms-to-rules rules [a b])
            -1
            1))
        update))

(comment
    ;; part 1
  (def data
    (load-data))

  (def rules
    (parse-ordering-rules data))

  (->> data
       parse-updates
       (filter #(conforms-to-rules rules %))
       (map middle-element)
       (map #(Integer/parseInt %))
       (reduce +))
  :rcf)

(comment
    ;; part 2
  (def data
    (load-data))

  (def rules
    (parse-ordering-rules data))

  (->> data
       parse-updates
       (remove #(conforms-to-rules rules %))
       (map #(make-conform rules %))
       (map middle-element)
       (map #(Integer/parseInt %))
       (reduce +))
  :rcf)