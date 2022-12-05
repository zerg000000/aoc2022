(ns aoc2022.day5
  (:require
    [clojure.java.io :as io]))


(defn read-lines
  [f]
  (->> (io/file f)
       (io/reader)
       (line-seq)
       (partition-by #(= "" %))))


(comment (read-lines "resources/day5.txt"))


(def stacks-lines
  (-> (read-lines "resources/day5.txt")
      first
      (drop-last)))


(defn read-stack-line
  [stack-line]
  (->> (partition-all 4 stack-line)
       (map second)))


(defn read-stacks
  [stacks-lines]
  (reduce
    (fn [acc line]
      (let [items (read-stack-line line)]
        (mapv
          (fn [stack item]
            (if-not (= item \space)
              (conj stack item)
              stack))
          acc
          items)))
    (vec (repeat (-> stacks-lines first count)
                 []))
    (-> stacks-lines drop-last reverse)))


(defn interpet
  [state op]
  (let [[move from to] op
        items (take-last move (get state from))]
    (-> state
        (update from #(vec (drop-last move %)))
        (update to into (reverse items)))))


(defn parse-op
  [line]
  (let [[_ move from to] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    [(parse-long move) (dec (parse-long from)) (dec (parse-long to))]))


(defn run-all
  [f mover-fn]
  (let [[stacks-lines _ ops-lines] (read-lines f)
        stacks (read-stacks stacks-lines)
        ops (map parse-op ops-lines)]
    (reduce
      mover-fn
      stacks
      ops)))


(comment 
  (read-stacks stacks-lines) 
  (->> (reduce
        interpet
        [[\Z \N]
         [\M \C \D]
         [\P]]
        [[1 1 0]
         [3 0 2]
         [2 1 0]
         [1 0 1]])
      (map peek)
      (apply str))
  ((interpet
   (read-stacks stacks-lines)
   [3 1 2]))
  )


(defn part-1
  []
  (->> (run-all "resources/day5.txt" interpet)
       (map peek)
       (apply str)))


(comment
  (part-1))


;; part 2

(defn interpet-cratemover-9001
  [state op]
  (let [[move from to] op
        items (take-last move (get state from))]
    (-> state
        (update from #(vec (drop-last move %)))
        (update to into items))))


(defn part-2
  []
  (->> (run-all "resources/day5.txt" interpet-cratemover-9001)
       (map peek)
       (apply str)))


(comment
  (part-2))
