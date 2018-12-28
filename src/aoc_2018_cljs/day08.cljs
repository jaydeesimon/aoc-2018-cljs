(ns aoc-2018-cljs.day08
  (:require [aoc-2018-cljs.util :refer [slurp-resource parse-int]]
            [clojure.string :as str]))


(def input (map parse-int (str/split (slurp-resource "day08.txt") #" ")))


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


(defn parse-license [input]
  (letfn [(parse-node [[qc qm & more]]
            (let [[children more] (nth (iterate parse-child [[] more]) qc)
                  [metadata more] (split-at qm more)]
              [{:children children, :metadata metadata} more]))
          (parse-child [[children more]]
            (let [[child more] (parse-node more)]
              [(conj children child) more]))]
    (first (parse-node input))))


(defn part-one [input]
  (let [root (parse-license input)]
    (tree-seq (comp seq :children) :children root)))


(comment

  ;; Part 1
  (reduce + (mapcat :metadata (part-one input)))


  ;; Part 2
  (letfn [(node-value [{:keys [metadata children]}]
            (if (not (seq children))
              (reduce + metadata)
              (->> metadata
                   (keep #(get children (dec %)))
                   (map node-value)
                   (reduce +))))]
    (node-value (parse-license input))))
