(ns aoc-2018-cljs.day08
  (:require [aoc-2018-cljs.util :refer [slurp-resource]]
            [clojure.string :as str]))


(def input (map #(js/parseInt %) (str/split (slurp-resource "day08.txt") #" ")))


(defn make-node [level metadata]
  {:level    level
   :metadata metadata})


(defn collect-nodes* [level nodes [qc qm & more]]
  (if (zero? qc)
    [(conj nodes (make-node level (take qm more)))
     (drop qm more)]
    (let [[nodes more] (reduce (fn [[nodes' more'] _]
                                 (collect-nodes* (inc level) nodes' more'))
                               [nodes more]
                               (range qc))]
      [(conj nodes (make-node level (take qm more)))
       (drop qm more)])))


(defn collect-nodes [ints]
  (first (collect-nodes* 0 [] ints)))


(comment

  ;; Part 1
  (reduce + (mapcat :metadata (collect-nodes input))))
