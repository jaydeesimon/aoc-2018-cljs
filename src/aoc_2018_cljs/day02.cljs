(ns aoc-2018-cljs.day02
  (:require [aoc-2018-cljs.util :refer [slurp-resource line-seq-resource]]))

(def input (line-seq-resource "day02.txt"))

(defn repeating-letters? [n s]
  (contains? (set (vals (frequencies s))) n))

(defn checksum [ids]
  (let [twos (filter (partial repeating-letters? 2) ids)
        threes (filter (partial repeating-letters? 3) ids)]
    (* (count twos) (count threes))))

(comment

  ;; Part 1
  (checksum input)



  )