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
              (lazy-seq
                (let [n (get input idx)
                      new-idx (+ idx n)
                      new-vec (update input idx f)]
                  (cons input
                        (when (and (not (neg? new-idx)) (< new-idx len))
                          (pt1* new-vec new-idx))))))]
      (count (pt1* (vec input) 0)))))

(defn pt1 [input]
  (maze input inc))

(defn pt2 [input]
  (maze input (fn [n]
                (if (>= n 3)
                  (dec n)
                  (inc n)))))
