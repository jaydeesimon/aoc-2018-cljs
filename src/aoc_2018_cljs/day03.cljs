(ns aoc-2018-cljs.day03
  (:require [aoc-2018-cljs.util :refer [line-seq-resource]]
            [clojure.set :as set]))


(defn parse-line [line]
  (let [[[_ & ns]] (re-seq #"(\d+) @ (\d+)\,(\d+)\: (\d+)x(\d+)" line)
        [id sx sy width length] (map #(js/parseInt %) ns)]
    {:id id :start [sx sy] :width width :length length}))


(def claims (map parse-line (line-seq-resource "day03.txt")))


(defn claim-coords [{[sx sy] :start width :width length :length}]
  (set
    (for [x (range width)
          y (range length)]
      [(+ sx x) (+ sy y)])))


(defn overlapping-coords [claims]
  (first
    (reduce (fn [[overlapping-coords quilt-coords] claim]
              (let [claim-coords (claim-coords claim)]
                [(set/union overlapping-coords (set/intersection claim-coords quilt-coords))
                 (set/union quilt-coords claim-coords)]))
            [#{} #{}]
            claims)))


(comment

  ;; Part 1
  (count (overlapping-coords claims))


  ;; Part 2
  (let [overlapping-coords (overlapping-coords claims)]
    (some (fn [claim]
            (when (empty? (set/intersection overlapping-coords (claim-coords claim)))
              claim))
          claims)))