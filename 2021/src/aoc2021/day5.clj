(ns aoc2021.day5
  (:require [clojure.string :as str]
            [aoc2021.util :as util]
            [sss.arrows :refer [->% ->>%]]
            [clojure.test :as t]))

(def example-data (str/trim "
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(defn parse-input [input-line]
  (->> (rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input-line))
       (map util/parse-long)
       (partition 2)
       (map vec)))

(defn ->points [[[x1 y1] [x2 y2]]]
  (letfn [(points-between [n1 n2]
            (if (> n2 n1)
              (range n1 (inc n2))
              (range n1 (dec n2) -1)))]
    (cond
      (= x1 x2) (map vector (repeat x1) (points-between y1 y2))
      (= y1 y2) (map vector (points-between x1 x2) (repeat y1))
      :else (map vector (points-between x1 x2) (points-between y1 y2)))))

(defn count-overlaps [points]
  (->> points
       frequencies
       (filter (->% val (>= 2)))
       (map key)
       count))

(defn p1 [input]
  (->> input
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2) (= y1 y2))))
       (mapcat ->points)
       count-overlaps))

(t/deftest test-p1
  (t/is (= 5 (->> (str/split-lines example-data) (map parse-input) p1)))
  (t/is (= 6283 (util/with-line-seq "day5.txt" (->>% (map parse-input) p1)))))

(defn p2 [input]
  (->> input
       (mapcat ->points)
       count-overlaps))

(t/deftest test-p2
  (t/is (= 12 (->> (str/split-lines example-data) (map parse-input) p2)))
  (t/is (= 18864 (util/with-line-seq "day5.txt" (->>% (map parse-input) p2)))))
