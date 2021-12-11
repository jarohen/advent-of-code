(ns aoc2021.util
  (:require [clojure.java.io :as io])
  (:import clojure.lang.MapEntry))

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

(defn ->neighbours [row-count col-count {:keys [diags?]}]
  (letfn [(->1d [row col]
            (+ (* row col-count) col))]
    (->> (for [row (range row-count)
               col (range col-count)
               :let [neighbours (->> (for [row* (range (dec row) (+ row 2))
                                           :when (<= 0 row* (dec row-count))
                                           col* (range (dec col) (+ col 2))
                                           :when (<= 0 col* (dec col-count))
                                           :when (not (and (= row row*) (= col col*)))
                                           :when (or diags? (= row row*) (= col col*))]
                                       (->1d row* col*))
                                     set)]]
           (MapEntry/create (->1d row col) neighbours))
         (into {}))))
