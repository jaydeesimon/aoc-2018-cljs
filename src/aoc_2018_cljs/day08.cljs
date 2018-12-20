(ns aoc-2018-cljs.day08
  (:require [aoc-2018-cljs.util :refer [slurp-resource]]
            [clojure.string :as str]))


(def input (map #(js/parseInt %) (str/split (slurp-resource "day08.txt") #" ")))


(defn make-node [metadata]
  {:metadata metadata})


(defn collect-nodes* [nodes [qc qm & more]]
  (if (zero? qc)
    [(conj nodes (make-node (take qm more)))
     (drop qm more)]
    (let [[nodes more] (reduce (fn [[nodes' more'] _]
                                 (collect-nodes* nodes' more'))
                               [nodes more]
                               (range qc))]
      [(conj nodes (make-node (take qm more)))
       (drop qm more)])))


(defn collect-nodes [ints]
  (first (collect-nodes* [] ints)))


(comment

  ;; Part 1
  (reduce + (mapcat :metadata (collect-nodes input))))
