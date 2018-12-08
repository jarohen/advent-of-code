(ns aoc2018.day6
  (:require [aoc2018.util :as util]
            [clojure.test :as t]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [sss.arrows :refer [->% ->>%]]))

(defn parse-lines [lines]
  (->> lines
       (map-indexed (fn [idx line]
                      (let [[x y] (->> (rest (re-matches #"(\d+), (\d+)" line))
                                       (map util/parse-long))]
                        {:idx idx, :x x, :y y})))))

(defn ->grid-size [cells]
  (inc (max (apply max (map :x cells))
            (apply max (map :y cells)))))

(defn ->cell-stats [cells {:keys [grid-size]}]
  (->> (for [i (range grid-size)
             j (range grid-size)]
         [i j])
       doall
       (pmap (fn [[i j]]
               (let [distances (for [{:keys [idx x y]} cells]
                                 {:idx idx,
                                  :d (+ (Math/abs (- i x))
                                        (Math/abs (- j y)))})

                     mins (->> distances
                               (sort-by :d)
                               (partition-by :d)
                               first)]
                 {:i i
                  :j j
                  :total-distance (transduce (map :d) + distances)

                  :closest-cell (when (= 1 (count mins))
                                  (:idx (first mins)))})))))

(defn edge-idxes [cell-stats {:keys [grid-size]}]
  (->> cell-stats
       (into #{} (comp (filter (some-fn #(contains? #{0 (dec grid-size)} (:i %))
                                        #(contains? #{0 (dec grid-size)} (:j %))))
                       (map :closest-cell)))))

(defn ->region-stats [cells {:keys [max-distance]}]
  (let [grid-size (->grid-size cells)
        cell-stats (->cell-stats cells {:grid-size grid-size})]
    {:biggest-region (-> (frequencies (map :closest-cell cell-stats))
                         (as-> freqs (apply dissoc freqs (edge-idxes cell-stats {:grid-size grid-size})))
                         (->> vals
                              (sort >)
                              first))

     :within-max-distance (->> cell-stats
                               (filter (fn [{:keys [total-distance]}]
                                         (< total-distance max-distance)))
                               count)}))

(t/deftest test-region-stats
  (t/is (= {:biggest-region 17, :within-max-distance 16})
        (->region-stats (parse-lines ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]) {:max-distance 32})))

(comment
  (util/with-line-seq "day6.txt"
    (fn [line-strs]
      (->region-stats (parse-lines line-strs) {:max-distance 10000}))))
