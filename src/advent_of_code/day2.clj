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
              (->> (sort row)
                   ((juxt last first))
                   (apply -))))
       (apply +)))

(defn pt2 [input]
  (->> input
       (map (fn [row]
              (reduce (fn [row-so-far el]
                        (if-let [divisor (first (filter #(or (zero? (mod el %)) (zero? (mod % el))) row-so-far))]
                          (reduced (/ (max divisor el) (min divisor el)))
                          (conj row-so-far el)))
                      []
                      row)))
       (apply +)))
