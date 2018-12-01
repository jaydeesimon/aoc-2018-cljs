(ns aoc-2018-cljs.day01
  (:require [aoc-2018-cljs.util :refer [slurp-resource line-seq-resource]]
            [clojure.set :as set]))

(def freq-changes (->> "day01.txt"
                       line-seq-resource
                       (map #(js/parseInt %))))

(comment

  ;; Part 1
  (reduce + freq-changes)

  ;; Part 2
  (reduce (fn [[past-freqs freq] freq-change]
            (let [freq (+ freq freq-change)]
              (if (contains? past-freqs freq)
                (reduced freq)
                [(set/union past-freqs #{freq}) freq])))
          [#{0} 0]
          (cycle freq-changes)))
