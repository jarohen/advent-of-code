(ns aoc2021.day6
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [aoc2021.util :as util]
            [clojure.java.io :as io])
  (:import clojure.lang.MapEntry))

(def example-data "3,4,3,1,2")

(defn parse-input [input]
  (->> (str/split input #",")
       (map util/parse-long)
       frequencies))

(def mapping
  {0 [1]
   1 [2]
   2 [3]
   3 [4]
   4 [5]
   5 [6]
   6 [0 7]
   7 [8]
   8 [0]})

(defn tick [fish]
  (->> (for [[new-k old-ks] mapping]
         (MapEntry/create new-k (transduce (keep fish) + 0 old-ks)))
       (into {})))

(defn count-fish [fish day]
  (->> (nth (iterate tick fish) day)
       vals
       (apply +)))

(t/deftest test-counting-fish
  (let [parsed-example (parse-input example-data)]
    (t/is (= 26 (count-fish parsed-example 18)))
    (t/is (= 5934 (count-fish parsed-example 80)))
    (t/is (= 26984457539 (count-fish parsed-example 256))))

  (let [parsed-input (parse-input (str/trim (slurp (io/resource "day6.txt"))))]
    (t/is (= 390011 (count-fish parsed-input 80)))
    (t/is (= 1746710169834 (count-fish parsed-input 256)))))
