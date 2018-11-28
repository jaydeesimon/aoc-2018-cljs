(ns aoc-2018-cljs.util
  (:require ["fs" :as fs]
            ["process" :as process]
            [goog.object :as goog]
            [clojure.string :as str]))


(defn slurp [path]
  (if (string? path)
    (fs/readFileSync path "utf8")
    (if (goog/isObject path)
      (let [type (.-type path)]
        (cond
          (identical? type "path")
          (fs/readFileSync (.-src path) "utf8")

          :else
          (throw (ex-info (str "Don't know what to do with: " path) {:file path}))))

      (throw (ex-info (str "Resource doesn't exist: " path) {:file path})))))


(defn line-seq [path]
  (str/split (slurp path) #"\n"))


(defn- resourcize [resource]
  (str (.cwd process) "/resources/" resource))


(defn slurp-resource [resource]
  (slurp (resourcize resource)))


(defn line-seq-resource [resource]
  (line-seq (resourcize resource)))
