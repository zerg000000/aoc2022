(ns aoc2022.day6)


(defn distinct-char-marker
  [s distinct-n]
  (->> s
       (seq)
       (reduce
         (fn [[processed remaining marker] ch]
           (let [processed' (conj processed ch)
                 last-n (take-last distinct-n processed')
                 uniq-count
                 (count (set last-n))]
             (cond
               marker
               [processed (conj remaining ch) true]
               (= distinct-n uniq-count)
               [processed' remaining true]
               :else
               [processed' remaining false])))
         [[] [] false])))


(comment
  (distinct-char-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)
  (distinct-char-marker "nppdvjthqldpwncqszvftbrmjlhg" 4)
  (distinct-char-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)
  (distinct-char-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4))


;; part 1

(defn start-of-packet-marker
  []
  (-> (distinct-char-marker (slurp "resources/day6.txt") 4)
      (first)
      (count)))


(comment
  (start-of-packet-marker))


;; part 2

(defn start-of-message-marker
  []
  (-> (distinct-char-marker (slurp "resources/day6.txt") 14)
      (first)
      (count)))


(comment
  (distinct-char-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
  (distinct-char-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)
  (distinct-char-marker "nppdvjthqldpwncqszvftbrmjlhg" 14)
  (distinct-char-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)
  (distinct-char-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)
  (start-of-message-marker))


;; done
