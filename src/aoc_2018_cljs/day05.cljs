(ns aoc-2018-cljs.day05
  (:require [aoc-2018-cljs.util :refer [slurp-resource]]
            [clojure.string :as str]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")


(def regex (let [v1 (map #(str % (.toUpperCase %)) alphabet)
                 v2 (map #(str (.toUpperCase %) %) alphabet)]
             (re-pattern (str/join "|" (concat v1 v2)))))


(defn react-polymer [polymer]
  (loop [polymer polymer]
    (let [reduced-polymer (str/replace-first polymer regex "")]
      (if (= polymer reduced-polymer)
        polymer
        (recur reduced-polymer)))))


(comment

  ;; Part 1
  (count (react-polymer (slurp-resource "day05.txt")))

  
  ;; Part 2
  (let [polymer (slurp-resource "day05.txt")]
    (->> alphabet
         (map (fn [letter]
                (let [re (re-pattern (str letter "+|" (.toUpperCase letter) "+"))]
                  (react-polymer (str/replace-all polymer re "")))))
         (apply min-key count)
         count)))