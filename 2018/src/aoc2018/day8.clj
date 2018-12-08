(ns aoc2018.day8
  (:require [aoc2018.util :as util]
            [clojure.test :as t]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn ->tree [input]
  (letfn [(->tree* [input]
            (let [[child-count metadata-count & input] input
                  [children input] (reduce (fn [[children input] _]
                                             (let [[child input] (->tree* input)]
                                               [(conj children child) input]))
                                           [[] input]
                                           (range child-count))
                  [metadata input] (split-at metadata-count input)]

              [{:children children
                :metadata metadata}
               input]))]
    (first (->tree* input))))

(defn sum [tree]
  (+ (reduce + (:metadata tree))
     (transduce (map sum) + (:children tree))))

(defn value [{:keys [children metadata]}]
  (if (zero? (count children))
    (reduce + metadata)

    (->> metadata
         (transduce (map (fn [idx]
                           (or (some-> (get children (dec idx)) value) 0)))
                    +))))

(t/deftest test-tree
  (let [tree (->tree [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])]
    (t/is (= 138 (sum tree)))
    (t/is (= 66 (value tree)))))

(comment
  (let [input (-> (slurp (io/resource "day8.txt"))
                  str/trim
                  (str/split #" ")
                  (->> (map util/parse-long)))
        tree (->tree input)]
    [(sum tree)
     (value tree)]))
