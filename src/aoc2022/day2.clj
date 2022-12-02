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

(def possiblities
  [["A" "X" :draw]
   ["B" "Y" :draw]
   ["C" "Z" :draw]
   ["A" "Y" :win]
   ["B" "Z" :win]
   ["C" "X" :win]
   ["A" "Z" :lose]
   ["B" "X" :lose]
   ["C" "Y" :lose]])

(def result-scores
  {:win 6
   :lose 0
   :draw 3})

;; all score combinations
(def scores
  (into {}
   (for [opponent opponent-ops
        me me-ops]
    [[opponent me] (+ (me-score me) 
                      (result-scores
                        (some (fn [[o m p]]
                                (when (= [o m] [opponent me])
                                  p))
                              possiblities)))]))
  )

;; part one

(defn total-score [inputs]
  (->> (map scores inputs)
       (reduce +)))

;; part two

(def result-code->result
  {"X" :lose
   "Y" :draw
   "Z" :win})

(defn score-fn
  [[opponent result-code]]
  (let [result (result-code->result result-code)]
    (+ (result-scores result)
       (->> possiblities
            (some (fn [[o m r]]
                    (when (and (= o opponent)
                               (= r result))
                      m)))
            (me-score)))))

(comment
  (score-fn ["A" "X"])
  
  (->> (map score-fn [["A" "Y"]
                      ["B" "X"]
                      ["C" "Z"]])
       (reduce +)))

(defn total-score-top-secret 
  [inputs]
  (->> (map score-fn inputs)
       (reduce +)))

