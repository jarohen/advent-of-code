(ns aoc2021.day3
  (:require [aoc2021.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->>%]]
            [clojure.string :as str]))

(def example-data
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(defn p1 [data]
  (let [freqs (for [idx (range (count (first data)))]
                (->> (frequencies (map #(nth % idx) data))
                     (sort-by val)
                     (map key)))]
    (->> [(map first freqs)
          (map second freqs)]
         (map (comp #(Long/parseLong % 2) str/join))
         (apply *))))

(t/deftest test-p1
  (t/is (= 198 (p1 example-data)))
  (t/is (= 1540244 (util/with-line-seq "day3.txt" p1))))

(defn p2 [data]
  (letfn [(p2* [f]
            (-> (reduce (fn [data idx]
                          (if (= 1 (count data))
                            (reduced (first data))
                            (->> (group-by #(nth % idx) data)
                                 (sort-by (juxt (comp count val) key))
                                 f
                                 val)))
                        data
                        (range))
                (Long/parseLong 2)))]
    (* (p2* first) (p2* second))))

(t/deftest test-p2
  (t/is (= 230 (p2 example-data)))
  (t/is (= 4203981 (p2 (util/with-line-seq "day3.txt" set)))))
