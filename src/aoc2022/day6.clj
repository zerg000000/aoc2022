(ns aoc2022.day6)


(defn first-marker
  [s num-of-uniq]
  (->> s
       (seq)
       (reduce
         (fn [[processed remaining marker] ch]
           (let [processed' (conj processed ch)
                 last-n (take-last num-of-uniq processed')
                 uniq-count
                 (count (set last-n))]
             (cond
               marker
               [processed (conj remaining ch) true]
               (= num-of-uniq uniq-count)
               [processed' remaining true]
               :else
               [processed' remaining false])))
         [[] [] false])))


(comment
  (first-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)
  (first-marker "nppdvjthqldpwncqszvftbrmjlhg" 4)
  (first-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)
  (first-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4))


;; part 1

(defn char-after-marker
  []
  (-> (first-marker (slurp "resources/day6.txt") 4)
      (first)
      (count)))


(comment
  (char-after-marker))


;; part 2

(defn char-after-message-marker
  []
  (-> (first-marker (slurp "resources/day6.txt") 14)
      (first)
      (count)))


(comment
  (first-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
  (first-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)
  (first-marker "nppdvjthqldpwncqszvftbrmjlhg" 14)
  (first-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)
  (first-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)
  (char-after-message-marker))


;; done
