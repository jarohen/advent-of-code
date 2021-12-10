(ns aoc2021.day9
  (:require [clojure.string :as str]
            [sss.arrows :refer [->>%]]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:import java.util.Comparator))

(def example-data (str/trim "
2199943210
3987894921
9856789892
8767896789
9899965678"))

(defn parse-input [input]
  (let [rows (str/split-lines input)
        row-count (count rows)]
    (->> (for [[row row-vals] (map vector (range) rows)
               :let [col-count (count row-vals)]
               [col v] (map vector (range) row-vals)]
           [[row col]
            {:v (Character/digit v 10)
             :neighbours (->> [[(dec row) col]
                               [(inc row) col]
                               [row (dec col)]
                               [row (inc col)]]
                              (filterv (every-pred (comp #(<= 0 % (dec row-count)) first)
                                                   (comp #(<= 0 % (dec col-count)) second))))}])
         (into {}))))

(defn ->low-points [input]
  (for [[cell {:keys [v neighbours]}] input
        :when (every? (fn [neighbour]
                        (< v (get-in input [neighbour :v])))
                      neighbours)]
    cell))

(defn p1 [input]
  (->> (for [low-point (->low-points input)]
         (inc (get-in input [low-point :v])))
       (apply +)))

(t/deftest test-p1
  (t/is (= 15 (p1 (parse-input example-data))))
  (t/is (= 535 (p1 (parse-input (str/trim (slurp (io/resource "day9.txt"))))))))

(defn p2 [input]
  (->> (for [low-point (->low-points input)]
         (loop [[cell & more-cells] (get-in input [low-point :neighbours])
                seen #{low-point}
                basin #{low-point}]
           (if-not cell
             (count basin)
             (let [{:keys [v neighbours]} (get input cell)]
               (if (= 9 v)
                 (recur more-cells (conj seen cell) basin)
                 (recur (distinct (concat more-cells (set/difference (set neighbours) seen)))
                        (conj seen cell)
                        (conj basin cell)))))))
       (sort (.reversed (Comparator/naturalOrder)))
       (take 3)
       (apply *)))

(t/deftest test-p2
  (t/is (= 1134 (p2 (parse-input example-data))))
  (t/is (= 1122700 (p2 (parse-input (str/trim (slurp (io/resource "day9.txt"))))))))
