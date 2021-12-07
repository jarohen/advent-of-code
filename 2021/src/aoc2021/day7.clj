(ns aoc2021.day7
  (:require [clojure.string :as str]
            [aoc2021.util :as util]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(def example-data "16,1,2,0,4,2,7,1,2,14")

(defn parse-input [input]
  (-> input (str/split #",") (->> (map util/parse-long))))

(def parsed-input
  (parse-input (str/trim (slurp (io/resource "day7.txt")))))

(defn solve [cost input]
  ;; HACK: there's probably a more direct mathematical way to do this
  ;; (it's not just the arithmetic mean, unfortunately)
  ;; but this runs in <5s on my machine, so meets my non-functionals for now ;)
  (->> (for [x (range (apply min input) (inc (apply max input)))]
         (->> (for [crab input]
                (cost crab x))
              (apply +)))
       (map vector (range))
       (sort-by second)
       first))

(defn p1-cost [crab x]
  (Math/abs (- crab x)))

(t/deftest test-p1
  (t/is (= [2 37] (solve p1-cost (parse-input example-data))))
  (t/is (= [329 340052] (solve p1-cost parsed-input))))

(defn p2-cost [crab x]
  (let [dist (Math/abs (- crab x))]
    (/ (* dist (inc dist)) 2)))

(t/deftest test-p2
  (t/is (= [5 168] (solve p2-cost (parse-input example-data))))
  (t/is (= [466 92948968] (solve p2-cost parsed-input))))
