(ns aoc2018.day14
  (:require [aoc2018.util :as util]
            [sss.arrows :refer [->% ->>%]]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn step [{:keys [recipes elf-idxes]}]
  (let [elf-scores (map recipes elf-idxes)
        total-score (reduce + elf-scores)
        recipes (into recipes
                      (if (>= total-score 10)
                        [(quot total-score 10) (rem total-score 10)]
                        [total-score]))]
    {:recipes recipes
     :elf-idxes (map (fn [elf-idx elf-score]
                       (mod (+ elf-idx elf-score 1)
                            (count recipes)))
                     elf-idxes
                     elf-scores)}))

(def init
  {:recipes [3 7]
   :elf-idxes [0 1]})

(defn p1 [steps]
  (-> (->> (iterate step init)
            (map :recipes)
            (drop-while (->% count (< (+ steps 10))))
            first)
      (subvec steps (+ steps 10))))

(t/deftest test-p1
  (t/are [steps expected] (= expected (str/join (p1 steps)))
    9 "5158916779"
    5 "0124515891"
    18 "9251071085"
    2018 "5941429882"))

(comment
  (str/join (p1 846601)))

(defn p2 [prefix]
  (->> (iterate step init)
       (map :recipes)
       (keep (fn [recipe]
               (let [start (max (- (count recipe) (count prefix) 3) 0)
                     index-of (java.util.Collections/indexOfSubList (subvec recipe start) prefix)]
                 (when (not= -1 index-of)
                   (+ start index-of)))))
       first))

(t/deftest test-p2
  (t/are [prefix expected] (= expected (p2 prefix))
    [5 1 5 8 9] 9
    [0 1 2 4 5] 5
    [9 2 5 1 0] 18
    [5 9 4 1 4] 2018))

(comment
  (p2 [8 4 6 6 0 1]))
