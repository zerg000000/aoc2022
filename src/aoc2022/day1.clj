(ns aoc2022.day1
  (:require [clojure.java.io :as io]))

(def input (-> (io/file "resources/day1.txt")
               (io/reader)
               (line-seq)))

;; each elf cal

(defn parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ -1)))

(comment
  (parse-int "0")
  (parse-int "bahbah")
  (parse-int "11"))

(defn calories-group-by-elf
  [input]
  (->> input
       (map parse-int)
       (partition-by pos-int?)
       (map #(reduce + %))
       (filter #(>= % 0))))

(defn top-calories
  [calories]
  (->> calories
       (calories-group-by-elf)
       (apply max)))

(comment
  (top-calories input))

;; top three

(defn top-three-calories
  [calories]
  (->> calories
       (calories-group-by-elf)
       (sort)
       (take-last 3)
       (reduce +)))

(comment
  (top-three-calories input))

;; done