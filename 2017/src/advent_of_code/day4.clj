(ns advent-of-code.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day4.txt")
       slurp s/trim s/split-lines
       (map #(s/split % #"\s"))))

(def pt1
  (->> input
       (filter (fn [line]
                 (= (count line) (count (set line)))))
       count))

(def pt2
  (->> input
       (filter (fn [line]
                 (= (count line) (count (into #{} (map frequencies) line)))))
       count))
