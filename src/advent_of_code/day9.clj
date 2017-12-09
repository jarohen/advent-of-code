(ns advent-of-code.day9
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/resource "day9.txt")))

(defn d9 [input]
  (loop [[c & more] input
         depth 0
         garbage? false
         garbage-count 0
         escape? false
         score 0]
    (cond
      (nil? c) [score garbage-count]

      escape? (recur more depth garbage? garbage-count false score)
      (= c \!) (recur more depth garbage? garbage-count true score)

      (and garbage? (= c \>)) (recur more depth false garbage-count false score)
      garbage? (recur more depth true (inc garbage-count) false score)
      (= c \<) (recur more depth true garbage-count false score)

      (= c \{) (recur more (inc depth) false garbage-count false score)
      (= c \}) (recur more (dec depth) false garbage-count false (+' score depth))

      :else (recur more depth garbage? garbage-count escape? score))))
