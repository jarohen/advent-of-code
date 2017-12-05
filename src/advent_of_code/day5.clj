(ns advent-of-code.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day5.txt")
       slurp s/trim s/split-lines
       (mapv #(Long/parseLong %))))

(defn maze [input f]
  (let [len (count input)]
    (letfn [(pt1* [input idx]
              (when (<= 0 idx (dec len))
                (lazy-seq
                  (cons input
                        (pt1* (update input idx f)
                              (+ idx (get input idx)))))))]
      (count (pt1* (vec input) 0)))))

(defn pt1 [input]
  (maze input inc))

(defn pt2 [input]
  (maze input (fn [n]
                (if (>= n 3)
                  (dec n)
                  (inc n)))))
