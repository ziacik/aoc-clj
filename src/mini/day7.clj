(ns mini.day7
  (:require
   [clojure.string :as string]))

(defn load-data []
  (->> (slurp "src/mini/day7.txt")
       string/split-lines))

(defn split-by-colon [line]
  (map string/trim (string/split line #":")))

(defn split-by-white [line]
  (map #(Long. (string/trim %))
       (string/split line #"\s+")))

(defn parse-lines [lines]
  (map
   (fn [line]
     (let [[head tail] (split-by-colon line)
           head-num (Long. head)
           tail-nums (map #(Long. %)
                          (split-by-white tail))]
       [head-num tail-nums]))
   lines))

(defn operate-two [[operand1 operand2]]
  (let [sum (+ operand1 operand2)
        mul (* operand1 operand2)]
    [sum mul]))

(defn concat-nums [num1 num2]
  (Long. (str num1 num2)))

(defn operate-two-ext [[operand1 operand2]]
  (let [sum (+ operand1 operand2)
        mul (* operand1 operand2)
        concat (concat-nums operand1 operand2)]
    [sum mul concat]))

(defmulti operate-two-multi (fn [type _] type))

(defmethod operate-two-multi :default [_ operands]
  (operate-two operands))

(defmethod operate-two-multi :extended [_ operands]
  (operate-two-ext operands))

(defn operate [operands type]
  (let [type (or type :default)]
    (cond
      (< (count operands) 2)
      operands

      (= 2 (count operands))
      (operate-two-multi type operands)

      :else
      (let [head (take 2 operands)
            tail (drop 2 operands)
            results (operate-two-multi type head)]
        (apply concat (map #(operate (cons % tail) type) results))))))

(defn can-operate? [target operands type]
  (some #(= target %) (operate operands type)))

(defn find-operables [lines type]
  (filter
   (fn [[target operands]]
     (can-operate? target operands type))
   lines))

(defn find-operables-results [lines type]
  (map
   (fn [[target]]
     target)
   (find-operables lines type)))

(comment
  ;; part 1

  (def data (load-data))
  (def lines (parse-lines data))

  (apply + (find-operables-results lines :default))

  :rcf)

(comment
  ;; part 2

  (def data (load-data))
  (def lines (parse-lines data))

  (apply + (find-operables-results lines :extended))

  :rcf)
