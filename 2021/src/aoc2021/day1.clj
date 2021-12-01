(ns aoc2021.day1
  (:require [aoc2021.util :as util]
            [clojure.test :as t]))

(def example-data
  [199 200 208 210 200 207 240 269 260 263])

(defn p1 [xs]
  (->> (partition 2 1 xs)
       (filter #(apply < %))
       count))

(t/deftest test-p1
  (t/is (= 7 (p1 example-data)))

  (t/is (= 1162
           (util/with-line-seq "day1.txt"
             (fn [xs]
               (p1 (map util/parse-long xs)))))))

(defn p2 [xs]
  (->> (partition 3 1 xs)
       (map #(apply + %))
       (p1)))

(t/deftest test-p2
  (t/is (= 5 (p2 example-data)))

  (t/is (= 1190
           (util/with-line-seq "day1.txt"
             (fn [xs]
               (p2 (map util/parse-long xs)))))))
