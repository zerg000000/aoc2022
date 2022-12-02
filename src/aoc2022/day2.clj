(ns aoc2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; hat would your total score be if everything goes exactly according to your strategy guide?
(defn read-pairs
  [f]
  (->> f
       (io/file)
       (io/reader)
       (line-seq)
       (map #(string/split % #" "))))

(def inputs (read-pairs "resources/day2.txt"))

(def opponent-ops ["A" "B" "C"])
(def me-ops ["X" "Y" "Z"])
(def op-score [1 2 3])

;; my op to score
(def me-score (zipmap me-ops op-score))

(comment
  (me-score "X")
  (me-score "Y")
  (me-score "Z"))

(def draw-pair
  {"A" "X"
   "B" "Y"
   "C" "Z"})

(def win-pair
  {"A" "Y"
   "B" "Z"
   "C" "X"})

(def lose-pair
  {"A" "Z"
   "B" "X"
   "C" "Y"})

;; all score combinations
(def scores
  (into {}
   (for [opponent opponent-ops
        me me-ops]
    [[opponent me] (+ (me-score me)
                    (cond
                      ;; draw
                      (= (draw-pair opponent) me) 3
                      ;; win
                      (= (win-pair opponent) me) 6
                      ;; lose
                      :else 0
                      ))]))
  )

;; part one

(def total-score
  (->> (map scores inputs)
       (reduce +)))

;; part two

;; opponent op :win|:loss|:draw -> me op

(defn score-fn
  [[opponent result]]
  (case result
    ;; lose
    "X" (+ 0 (-> opponent lose-pair (me-score)))
    ;; draw
    "Y" (+ 3 (-> opponent draw-pair (me-score)))
    ;; win
    "Z" (+ 6 (-> opponent win-pair (me-score)))))

(comment
  (score-fn ["A" "X"])

  (draw-pair "A")
  
  (->> (map score-fn [["A" "Y"]
                      ["B" "X"]
                      ["C" "Z"]])
       (reduce +)))

(->> (map score-fn inputs)
     (reduce +))

