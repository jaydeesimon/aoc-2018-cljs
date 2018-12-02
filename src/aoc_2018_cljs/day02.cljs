(ns aoc-2018-cljs.day02
  (:require [aoc-2018-cljs.util :refer [slurp-resource line-seq-resource]]))

(def input (line-seq-resource "day02.txt"))


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
       (remove nil?)))


(comment

  ;; Part 1
  (checksum input)

  ;; Part 2
  (->> (sort input)
       (partition 2 1)
       (filter (fn [[s1 s2]]
                 (= 1 (difference s1 s2))))
       (first)
       (apply common-letters)
       (apply str)))