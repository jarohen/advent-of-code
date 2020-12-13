(ns aoc2020.util
  (:require [clojure.java.io :as io]))

(defn with-line-seq [resource f]
  (with-open [rdr (io/reader (io/resource resource))]
    (f (line-seq rdr))))

(defn parse-long [s] (Long/parseLong s))

(def directions
  (set (for [dx [-1 0 1]
             dy [-1 0 1]
             :when (not (and (zero? dx) (zero? dy)))]
         [dx dy])))

(defn fixpoint [f x]
  (->> (iterate f x)
       (partition-all 2 1)
       (take-while (fn [[a b]]
                     (not= a b)))
       last
       second))
