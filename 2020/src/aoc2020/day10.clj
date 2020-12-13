(ns aoc2020.day10
  (:require [aoc2020.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->>%]]))

(def test-case1 [16 10 15 5 1 11 7 19 6 12 4])
(def test-case2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn p1 [adapters]
  (let [freqs (->> (for [[lo hi] (->> (cons 0 (sort adapters))
                                      (partition 2 1))]
                     (- hi lo))
                   frequencies)]
    (* (inc (get freqs 3))
       (get freqs 1))))

(defn p2 [adapters]
  (-> (cons 0 adapters)
      (->> (sort (comp - compare))
           (reduce (fn [perms adapter]
                     (assoc perms adapter (if (empty? perms)
                                            1
                                            (->> (keep perms (range (+ adapter 1) (+ adapter 4)))
                                                 (reduce +)))))
                   {}))
      (get 0)))



(t/deftest test-d10
  (t/is (= 35 (p1 test-case1)))
  (t/is (= 220 (p1 test-case2)))

  (t/is (= 8 (p2 test-case1)))
  (t/is (= 19208 (p2 test-case2)))

  (t/is (= [3000 193434623148032]
           (util/with-line-seq "day10.txt"
             (fn [lines]
               (let [xs (map #(Long/parseLong %) lines)]
                 [(p1 xs) (p2 xs)]))))))
