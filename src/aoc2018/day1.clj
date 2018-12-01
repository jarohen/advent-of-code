(ns aoc2018.day1
  (:require [aoc2018.files :as files]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]))

(defn with-input [f]
  (files/with-line-seq "day1.txt" (->>% (map #(Long/parseLong %)) f)))

(defn part-a [nums]
  (reduce + nums))

(defn part-b [nums]
  (->> (cycle nums)
       (reductions +)
       (reduce (fn [acc el]
                 (if (contains? acc el)
                   (reduced el)
                   (conj acc el)))
               #{0})))

(comment
  (with-input (->>% part-b)))

(t/deftest test-part-b
  (t/are [e nums] (= e (part-b nums))
    0 [1 -1]
    10 [3 3 4 -2 -4]
    5 [-6 3 8 5 -6]
    14 [7 7 -2 -7 -4]))
