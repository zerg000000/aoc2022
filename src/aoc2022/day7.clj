(ns aoc2022.day7
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]))


(defn read-lines
  [f]
  (->> (io/file f)
       (io/reader)
       (line-seq)))


(defn read-ls
  [lines]
  (loop [lines lines
         dir-info {}]
    (let [line (first lines)]
      (cond
        (or
          (nil? line)
          (string/starts-with? line "$"))
        [dir-info lines]
        (string/starts-with? line "dir")
        (let [[_ dir] (re-find #"dir ([\w\.]+)" line)]
          (recur (next lines) (assoc dir-info dir [:dir dir])))
        :else
        (let [[_ size filename] (re-find #"(\d+) ([\w\.]+)" line)]
          (recur (next lines) (assoc dir-info filename [:file filename (parse-long size)])))))))


(comment
  (read-ls ["12 jj.dd"
            "dir abc"
            "1234 grc.1"])
  (read-ls ["12 jj.dd"
            "dir abc"
            "1234 grc.1"
            "$ cd .."]))


(defn parse-cd
  [line pwd]
  (let [[_ dir] (re-find #"\$ cd (.+)" line)
        pwd'    (if (= dir "..")
                  (pop pwd)
                  (conj pwd dir))]
    pwd'))


(comment
  (parse-cd "$ cd .." ["/" "grct"]))


(defn build-file-tree
  [lines root]
  (loop [tree {}
         lines lines
         pwd root]
    (if-let [line (first lines)]
      (cond
        (= line "$ ls")
        (let [[ls-info ops-left] (read-ls (next lines))]
          (recur (assoc tree pwd ls-info) ops-left pwd))
        (string/starts-with? line "$ cd ..")
        (recur tree (next lines) (if (> (count pwd) 1)
                                   (pop pwd)
                                   pwd))
        (string/starts-with? line "$ cd /")
        (recur tree (next lines) root)
        (string/starts-with? line "$ cd")
        (recur tree (next lines) (parse-cd line pwd))
        :else tree)
      tree)))


(comment
  (def fs
    (-> "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"
        (string/split-lines)
        (build-file-tree ["/"]))))


(defn total-size
  [fs dir]
  (->> (get fs dir)
       (reduce-kv
         (fn [acc _ [typ nam size]]
           (+ acc (case typ
                    :file size
                    :dir (total-size fs (conj dir nam)))))
         0)))



(comment
  fs
  (total-size fs ["/"])
  (->> (keys fs)
       (map #(total-size fs %))
       (filter #(<= % 100000))
       (reduce +)))


;; part 1

(defn sum-of-small-dir
  [f]
  (let [fs (-> (read-lines f)
               (build-file-tree ["/"]))]
    (->> (keys fs)
         (map #(total-size fs %))
         (filter #(<= % 100000))
         (reduce +))))


(comment
  (sum-of-small-dir "resources/day7.txt"))


;; part 2

(def total-disk-space 70000000)
(def require-free-space 30000000)


(defn enough-for-update
  [f]
  (let [fs (-> (read-lines f)
               (build-file-tree ["/"]))
        current-free-space (- total-disk-space (total-size fs ["/"]))
        free-up (- require-free-space current-free-space)]
    (->> (keys fs)
         (map #(total-size fs %))
         (filter #(<= free-up %))
         (sort)
         (first))))


(comment
  (enough-for-update "resources/day7.txt"))

