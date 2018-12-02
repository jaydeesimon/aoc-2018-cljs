(ns aoc-2018-cljs.day02
  (:require [aoc-2018-cljs.util :refer [slurp-resource line-seq-resource]]))


(def ids (line-seq-resource "day02.txt"))


(defn repeating-letters? [n s]
  (contains? (set (vals (frequencies s))) n))


(defn checksum [ids]
  (let [twos (filter (partial repeating-letters? 2) ids)
        threes (filter (partial repeating-letters? 3) ids)]
    (* (count twos) (count threes))))


(defn difference [s1 s2]
  (->> (map (fn [c1 c2]
              (if (not= c1 c2) 1 0))
            s1 s2)
       (reduce +)))


(defn common-letters [s1 s2]
  (->> (map (fn [c1 c2]
              (when (= c1 c2)
                c1))
            s1 s2)
       (remove nil?)
       (apply str)))


;; not the most efficient way to create pairs
;; of each element of xs to every other element
;; but fast enough for our input
(defn pairs [xs]
  (->> (for [i xs
             j xs
             :when (not= i j)]
         (set [i j]))
       (into #{})
       (map vec)))


(defn index-by-difference [ids]
  (group-by (fn [[id1 id2]]
              (difference id1 id2))
            (pairs ids)))


(comment

  ;; Part 1
  (checksum ids)

  ;; Part 2
  (as-> (index-by-difference ids) ?
        (get ? 1) ;; get the pairs that are off-by-one
        (first ?)
        (apply common-letters ?)))