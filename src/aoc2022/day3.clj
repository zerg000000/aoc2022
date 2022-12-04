(ns aoc2022.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn read-lines [f]
  (-> (io/file f)
      (io/reader)
      (line-seq)))

(defn item-type [[f s]]
  (first (set/intersection (set f) (set s))))

(defn type->priority
  [t]
  (let [c (int t)]
    (if (>= c (int \a))
      (+ (- c (int \a)) 1)
      (+ (- c (int \A)) 1 26))))

(defn ->compartments
  [rucksack]
  (let [chars (seq rucksack)
        len (count chars)
        result (partition (/ len 2) chars)]
    result))

(comment
  (- (int \p) (int \a))
  (int \A)
  (int \p)
  (type->priority \p)
  (type->priority \L)
  (type->priority \P)
  (type->priority \v))

;; part 1

(defn sum-of-priority
  [f]
  (->> (read-lines f)
       (map ->compartments)
       (map item-type)
       (map type->priority)
       (reduce +)))

(comment
  (sum-of-priority "resources/day3.txt"))

;; part 2

(defn sum-of-group-priority
  [f]
  (->> (read-lines f)
       (map seq)
       (partition 3)
       (map (fn [[a b c]]
              (set/intersection (set a) (set b) (set c))))
       (map first)
       (map type->priority)
       (reduce +)))

(comment
  (sum-of-group-priority "resources/day3.txt"))
