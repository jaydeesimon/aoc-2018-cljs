(ns aoc-2018-cljs.day08
  (:require [aoc-2018-cljs.util :refer [slurp-resource]]
            [clojure.string :as str]))


(def input (map #(js/parseInt %) (str/split (slurp-resource "day08.txt") #" ")))


(defn collect-metadata* [metadata [qc qm & more]]
  (if (zero? qc)
    [(into metadata (take qm more))
     (drop qm more)]
    (let [[metadata more] (reduce (fn [[metadata' more'] _]
                                    (collect-metadata* metadata' more'))
                                  [metadata more]
                                  (range qc))]
      [(into metadata (take qm more))
       (drop qm more)])))


(defn collect-metadata [ints]
  (first (collect-metadata* [] ints)))


(comment

  ;; Part 1
  (reduce + (collect-metadata input)))
