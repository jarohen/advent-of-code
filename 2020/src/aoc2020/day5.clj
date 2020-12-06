(ns aoc2020.day5
  (:require [aoc2020.util :as util]
            [clojure.test :as t]))

(do
  (defn decode [chs hi]
    (reduce (fn [acc el]
              (cond-> (bit-shift-left acc 1)
                (= el hi) inc))
            0
            chs))

  (defn parse-seat [s]
    (let [[row col] (split-at 7 s)]
      [(decode row \B) (decode col \R)]))

  (defn seat-id [[row col]]
    (+ (* 8 row) col))

  (util/with-line-seq "day5.txt"
    (fn [lines]
      (let [seat-ids (sort (for [line lines]
                             (seat-id (parse-seat line))))]
        [(last seat-ids)
         (first (for [[s1 s2] (partition-all 2 1 seat-ids)
                      :when (not= 1 (- s2 s1))]
                  [s1 s2]))]))))

(t/deftest test-p1
  (t/is (= [44 5] (parse-seat "FBFBBFFRLR")))
  (t/is (= [70 7] (parse-seat "BFFFBBFRRR")))
  (t/is (= [14 7] (parse-seat "FFFBBBFRRR")))
  (t/is (= [102 4] (parse-seat "BBFFBBFRLL")))
  )
