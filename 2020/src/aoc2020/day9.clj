(ns aoc2020.day9
  (:require [aoc2020.util :as util]
            [clojure.test :as t]))

(defn p1 [xs preamble-size]
  (first (for [batch (->> xs
                          (partition (inc preamble-size) 1))
               :let [[prev [check]] (split-at preamble-size batch)]
               :when (empty? (for [x prev
                                   y prev
                                   :when (not= x y)
                                   :when (= (+ x y) check)]
                               [x y]))]
           check)))

(defn p2 [xs goal]
  (->> (for [sub-seq (->> (iterate rest xs)
                          (take-while seq))
             [sum els] (->> (reductions (fn [[acc coll] el]
                                          [(+ acc el) (conj coll el)])
                                        [0 []]
                                        sub-seq)
                            rest
                            (take-while (comp #(<= % goal) first)))
             :when (= sum goal)
             :when (> (count els) 1)]
         (+ (apply min els) (apply max els)))
       first))

(def test-case
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(t/deftest test-d9
  (t/is (= 127 (p1 test-case 5)))
  (t/is (= 62 (p2 test-case 127))))

(comment
  (util/with-line-seq "day9.txt"
    (fn [lines]
      (let [xs (map #(Long/parseLong %) lines)
            p1-soln (p1 xs 25)]
        [p1-soln (p2 xs p1-soln)]))))[400480901 67587168]
