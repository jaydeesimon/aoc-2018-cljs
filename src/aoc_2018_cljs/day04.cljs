(ns aoc-2018-cljs.day04
  (:require [aoc-2018-cljs.util :refer [line-seq-resource]]
            [clojure.string :as str]))


(defn parse-line [line]
  (let [line-re #"\[\d\d\d\d\-(\d\d)\-(\d\d) (\d\d):(\d\d)\] (.+)$"
        [_ & ns] (re-find line-re line)
        [month day hour minute] (map #(js/parseInt %) (butlast ns))
        description (last ns)
        [_ guard] (re-find #"Guard #(\d+) begins shift" description)]
    {:month       month
     :day         day
     :hour        hour
     :minute      minute
     :guard       guard
     :state       (if (or (str/starts-with? description "Guard")
                          (str/starts-with? description "wakes"))
                    :awake
                    :asleep)
     :description description}))


;; After parsing the lines, not every line identifies
;; the guard associated with the event. This function
;; makes sure every event has an associated guard.
;; The events must be sorted by the timestamp
;; for it to work correctly.
(defn fill-in-guard [events]
  (let [initial-guard (get (first events) :guard)
        current-guard (atom initial-guard)]
    (map (fn [{:keys [guard] :as event}]
           (when (and guard (not= guard @current-guard))
             (reset-vals! current-guard guard))
           (assoc event :guard @current-guard))
         events)))


(defn normalize-events [filename]
  (->> (line-seq-resource filename)
       (map parse-line)
       (sort-by (juxt :month :day :hour :minute))
       fill-in-guard))

  
(defn asleep-diffs [events]
  (->> (partition 2 1 events)
       (filter (fn [[{state1 :state} {state2 :state}]]
                 (and (= state1 :asleep) (= state2 :awake))))
       (map (fn [[{minute1 :minute guard :guard} {minute2 :minute}]]
              {:guard        guard
               :start-minute minute1
               :end-minute   minute2}))))


(defn compress* [diffs]
  (reduce (fn [asleep-map {:keys [guard start-minute end-minute]}]
            (update
              asleep-map
              guard
              (fn [{cur-duration :duration cur-hour-minutes-asleep :hour-minutes-asleep :as asleep-info}]
                (let [duration (- end-minute start-minute)
                      hour-minutes-asleep (range start-minute end-minute)]
                  (if (nil? asleep-info)
                    {:duration            duration
                     :hour-minutes-asleep hour-minutes-asleep}
                    {:duration            (+ duration cur-duration)
                     :hour-minutes-asleep (concat cur-hour-minutes-asleep hour-minutes-asleep)})))))
          {}
          diffs))


(defn find-guard-with-max-sleep [compressed-asleep-diffs]
  (first (apply max-key (comp :duration second) compressed-asleep-diffs)))


(defn find-day-most-asleep [compressed-asleep-diffs guard]
  (->> (get-in compressed-asleep-diffs [guard :hour-minutes-asleep])
       frequencies
       (apply max-key val)
       first))


(defn compress-asleep-diffs [filename]
  (->> (normalize-events filename)
       asleep-diffs
       compress*))


(defn part-one [filename]
  (let [compressed-asleep-diffs (compress-asleep-diffs filename)
        guard (find-guard-with-max-sleep compressed-asleep-diffs)
        day-most-asleep (find-day-most-asleep compressed-asleep-diffs guard)]
    {:guard  guard
     :answer (* (js/parseInt guard) day-most-asleep)}))


(defn find-guard-with-most-frequent-asleep-minute [compressed-asleep-diffs]
  (->> compressed-asleep-diffs
       (map (fn [[guard {:keys [hour-minutes-asleep]}]]
              {:guard guard :max (apply max-key val (frequencies hour-minutes-asleep))}))
       (apply max-key (comp second :max))))


(defn part-two [filename]
  (let [compressed-asleep-diffs (compress-asleep-diffs filename)
        {guard :guard [minute _] :max} (find-guard-with-most-frequent-asleep-minute compressed-asleep-diffs)]
    {:guard guard :answer (* (js/parseInt guard) minute)}))


(comment

  ;; Part 1
  (part-one "day04.txt")


  ;; Part 2
  (part-two "day04.txt"))