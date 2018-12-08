(ns aoc2018.day2
  (:require [aoc2018.util :as util]
            [clojure.test :as t]))

(defn checksum [ids]
  (let [{:keys [twos threes]} (->> ids
                                   (reduce (fn [{:keys [twos threes]} id]
                                             (let [freqs (set (vals (frequencies id)))]
                                               {:twos (+ twos (if (contains? freqs 2) 1 0))
                                                :threes (+ threes (if (contains? freqs 3) 1 0))}))
                                           {:twos 0, :threes 0}))]
    (* twos threes)))

(t/deftest test-checksum
  (t/is (= 12 (checksum ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]))))

(comment
  (util/with-line-seq "day2.txt" checksum))

(defn different-characters [id1 id2]
  (count (filter true? (map not= id1 id2))))

(defn similar [ids]
  (->> (for [id1 ids
             id2 ids
             :when (and (not= id1 id2)
                        (= 1 (different-characters id1 id2)))]
         #{id1 id2})

       first))

(t/deftest find-similar-pair
  (t/is (= #{"fghij" "fguij"}
           (similar ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]))))

(comment
  (util/with-line-seq "day2.txt" similar))
