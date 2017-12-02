(ns advent-of-code.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day2.txt")
       slurp s/trim s/split-lines
       (map (comp (partial map #(Long/parseLong %))
                  #(s/split % #"\s")))))

(defn pt1 [input]
  (->> input
       (map (fn [row]
              (- (apply max row) (apply min row))))
       (apply +)))

(defn pt2 [input]
  (->> input
       (map (fn [row]
              (->> (for [x row, y row]
                     [x y])

                   (keep (fn [[x y]]
                           (when (and (not= x y) (zero? (mod x y)))
                             (/ x y))))

                   first)))
       (apply +)))
