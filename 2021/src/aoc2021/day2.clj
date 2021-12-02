(ns aoc2021.day2
  (:require [aoc2021.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->>%]]))

(def example-data
  ["forward 5"
   "down 5"
   "forward 8"
   "up 3"
   "down 8"
   "forward 2"])

(defn parse-direction [direction]
  (let [[_ dir steps] (re-matches #"(\w+) (\d+)" direction)]
    [(keyword dir) (util/parse-long steps)]))

(def dirs
  {:forward [1 0]
   :up [0 -1]
   :down [0 1]})

(defn p1 [directions]
  (reduce (fn [loc [dir steps]]
            (->> (mapv #(* steps %) (get dirs dir))
                 (mapv + loc)))
          [0 0]
          directions))

(t/deftest test-p1
  (t/is (= [15 10] (p1 (map parse-direction example-data))))
  (t/is (= [1967 1031] (util/with-line-seq "day2.txt" (->>% (map parse-direction) p1)))))

(defn p2 [directions]
  (reduce (fn [[x y aim] [dir mag]]
            (case dir
              :forward [(+ x mag) (+ y (* aim mag)) aim]
              :up [x y (- aim mag)]
              :down [x y (+ aim mag)]))
          [0 0 0]
          directions))

(t/deftest test-p2
  (t/is (= [15 60 10] (p2 (map parse-direction example-data))))
  (t/is (= [1967 967791 1031] (util/with-line-seq "day2.txt" (->>% (map parse-direction) p2)))))
