(ns aoc2022.day4
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]))


(defn read-lines
  [f]
  (->> (io/file f)
       (io/reader)
       (line-seq)))


(defn ->pairs
  [s]
  (string/split s #","))


(comment
  (->> (read-lines "resources/day4.txt") (map ->pairs)))


(defn parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ -1)))


(defn ->assignment
  [r]
  (let [[start end] (string/split r #"-")]
    (range (parse-int start) (inc (parse-int end)))))


(defn ->assigments
  [[first second]]
  [(->assignment first)
   (->assignment second)])


(defn fully-contains?
  [[first-range second-range]]
  (or
    (set/superset? (set first-range) (set second-range))
    (set/superset? (set second-range) (set first-range))))


(comment
  (->> (read-lines "resources/day4.txt") 
       (map ->pairs) 
       (map ->assigments)
       (map fully-contains?)
       (filter true?)
       (count)))


;; part 1

(defn count-fully-contain
  [pairs]
  (->> pairs
       (map ->assigments)
       (map fully-contains?)
       (filter true?)
       (count)))


;; part 2

(defn overlap?
  [[first-range second-range]]
  (set/intersection (set first-range) (set second-range)))


(defn count-overlap
  [pairs]
  (->> pairs
       (map ->assigments)
       (map overlap?)
       (filter #(> (count %) 0))
       (count)))


(comment
  (count-overlap
   [["5-7" "7-9"]
    ["2-8" "3-7"]
    ["6-6" "4-6"]
    ["2-6" "4-8"]]) 
  (->> (read-lines "resources/day4.txt")
       (map ->pairs)
       (count-overlap)))


;; done
