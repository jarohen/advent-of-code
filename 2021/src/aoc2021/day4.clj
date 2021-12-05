(ns aoc2021.day4
  (:require [aoc2021.util :as util]
            [clojure.string :as str]
            [sss.arrows :refer [->>%]]
            [clojure.test :as t]))

(def example-data (str/trim "
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"))

(defn parse-input [input-lines]
  (let [[selections & boards] input-lines]
    {:selections (->> (str/split selections #",") (map util/parse-long))
     :boards (for [board (->> (partition-all 6 boards) (map rest))]
               (let [row-wise (for [row board]
                                (-> (str/trim row)
                                    (str/split #"\s+")
                                    (->> (map util/parse-long))))]
                 {:unmatched (into #{} (mapcat seq) row-wise)
                  :match-sets (concat (into #{} (map set) row-wise)
                                      (into #{} (apply map hash-set row-wise)))}))}))

(defn advance-board [{:keys [unmatched match-sets matched]} selection]
  {:unmatched (disj unmatched selection)
   :match-sets (for [match-set match-sets]
                 (disj match-set selection))
   :matched (cond-> (or matched #{})
              (contains? unmatched selection) (conj selection))})

(defn winner? [{:keys [match-sets] :as board}]
  (when (some empty? match-sets)
    board))

(defn p1 [{:keys [selections boards]}]
  (let [{:keys [winner last-call]}
        (reduce (fn [boards selection]
                  (let [boards (map #(advance-board % selection) boards)]
                    (if-let [winner (some winner? boards)]
                      (reduced {:winner winner, :last-call selection})
                      boards)))
                boards
                selections)]
    (* last-call (apply + (:unmatched winner)))))

(t/deftest test-p1
  (t/is (= 4512 (p1 (parse-input (str/split-lines example-data)))))
  (t/is (= 63552 (util/with-line-seq "day4.txt" (->>% parse-input p1)))))

(defn p2 [{:keys [selections boards]}]
  (let [{:keys [loser last-call]}
        (reduce (fn [boards selection]
                  (let [boards (->> boards
                                    (map #(advance-board % selection)))]
                    (if (and (= 1 (count boards)) (winner? (first boards)))
                      (reduced {:loser (first boards)
                                :last-call selection})
                      (remove winner? boards))))
                boards
                selections)]
    (* last-call (apply + (:unmatched loser)))))

(t/deftest test-p2
  (t/is (= 1924 (p2 (parse-input (str/split-lines example-data)))))
  (t/is (= 9020 (util/with-line-seq "day4.txt" (->>% parse-input p2)))))
