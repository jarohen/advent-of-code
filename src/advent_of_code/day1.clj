(ns advent-of-code.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day1.txt") slurp s/trim (map (comp #(Long/parseLong %) str))))

(defn sum-matching [pairs]
  (->> pairs
       (transduce (comp (filter #(apply = %))
                        (map first))
                  + 0)))

(defn pt1 [input]
  (->> (concat input [(first input)])
       (partition 2 1)
       sum-matching))

(defn pt2 [input]
  (->> input
       (split-at (/ (count input) 2))
       (apply map vector)
       sum-matching
       (* 2)))
