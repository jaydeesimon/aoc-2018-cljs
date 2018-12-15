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


(defn find-next-tasks [{:keys [nodes] :as g} done?]
  (sort (filter (fn [node]
                  (and (dependencies-met? g done? node)
                       (not (contains? done? node))))
                nodes)))


(defn part-one [filename]
  (let [g (build-graph (parse-lines filename))]
    (loop [steps []
           done? #{:start}]
      (let [next-step (first (find-next-tasks g done?))]
        (if (nil? next-step)
          (apply str steps)
          (recur (conj steps next-step)
                 (into done? next-step)))))))


(defn task-seconds [task]
  (+ 60 (inc (- (.charCodeAt task) (.charCodeAt "A")))))


(defn assign-tasks
  [worker-state current-time avail-tasks]
  (let [avail-workers (->> worker-state
                           (filter (comp nil? second))
                           (map first))]
    (reduce (fn [worker-state [avail-worker avail-task]]
              (assoc worker-state avail-worker {:task avail-task :time-started current-time}))
            worker-state
            (map #(vector %1 %2) avail-workers avail-tasks))))


(defn busy-workers [worker-state]
  (filter (comp not nil? second) worker-state))


(defn tasks-in-progress [worker-state]
  (set (filter some? (map (comp :task second) worker-state))))


(defn progress-done
  [{:keys [worker-state] :as state} current-time]
  (let [times-up? (fn [task time-started]
                    (= (task-seconds task) (- current-time time-started)))]
    (reduce (fn [state [worker {:keys [task time-started]}]]
              (if (times-up? task time-started)
                (-> (assoc-in state [:worker-state worker] nil)
                    (update :done? #(into % task)))
                state))
            state
            (busy-workers worker-state))))



(defn initialize-state [workers]
  {:worker-state (into {} (map #(vector % nil) (range workers)))
   :done? #{:start}})


(defn part-two [filename workers]
  (let [g (build-graph (parse-lines filename))]
    (loop [state (initialize-state workers)
           current-time 0]
      (let [{:keys [worker-state done?]} (progress-done state current-time)
            tasks-in-progress (tasks-in-progress worker-state)
            avail-tasks (sort (remove tasks-in-progress (find-next-tasks g done?)))
            worker-state+ (assign-tasks worker-state current-time avail-tasks)]
        (if (not (seq (busy-workers worker-state+)))
          current-time
          (recur {:worker-state worker-state+ :done? done?}
                 (inc current-time)))))))




(comment

  ;; Part 1
  (part-one "day07.txt")


  ;; Part 2
  (part-two "day07.txt" 5))


