(ns aoc-2018-cljs.day07
  (:require [aoc-2018-cljs.util :refer [line-seq-resource]]
            [clojure.set :as set]))

(defn parse-lines [filename]
  (map (fn [line]
         (let [re #"Step (\w) must be finished before step (\w) can begin\."
               [_ child parent] (re-find re line)]
           [parent child]))
       (line-seq-resource filename)))


(defn next-steps [edges]
  (reduce (fn [graph [parent child]]
            (merge-with into graph {child #{parent}}))
          {}
          edges))


(defn dependencies [edges]
  (reduce (fn [graph [parent child]]
            (merge-with into graph {parent #{child}}))
          {}
          edges))

(defn build-graph [edges]
  (let [nodes (set (flatten edges))
        starts (set/difference nodes (set (map first edges)))]
    {:next-steps   (assoc (next-steps edges) :start starts)
     :dependencies (reduce
                     (fn [deps start]
                       (assoc deps start #{:start}))
                     (dependencies edges)
                     starts)
     :nodes        nodes}))


(defn dependencies-met? [{:keys [dependencies]} done? node]
  (set/subset? (get dependencies node #{nil}) done?))


(defn find-next [{:keys [nodes] :as g} done?]
  (first (sort (filter (fn [node]
                         (and (dependencies-met? g done? node)
                              (not (contains? done? node))))
                       nodes))))

(defn part-one [filename]
  (let [g (build-graph (parse-lines filename))]
    (loop [steps []
           done? #{:start}]
      (let [next-step (find-next g done?)]
        (if (nil? next-step)
          (apply str steps)
          (recur (conj steps next-step)
                 (into done? next-step)))))))


(comment

  ;; Part 1
  (part-one "day07.txt"))


