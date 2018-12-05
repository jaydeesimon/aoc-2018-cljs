(ns aoc-2018-cljs.day05
  (:require [aoc-2018-cljs.util :refer [slurp-resource]]
            [clojure.string :as str]))


(def regex (let [alphabet "abcdefghijklmnopqrstuvwxyz"
                 v1 (map #(str % (.toUpperCase %)) alphabet)
                 v2 (map #(str (.toUpperCase %) %) alphabet)]
             (re-pattern (str/join "|" (concat v1 v2)))))


(defn reduce-polymer [polymer]
  (loop [polymer polymer]
    (let [reduced-polymer (str/replace-first polymer regex "")]
      (if (= polymer reduced-polymer)
        polymer
        (recur reduced-polymer)))))


(comment

  ;; Part 1
  (count (reduce-polymer (slurp-resource "day05.txt")))

  )