(ns aoc2021.day13
  (:require [clojure.string :as str]
            [aoc2021.util :as util]
            [clojure.test :as t]))

(defn parse-input [input-lines]
  (let [[cells more-lines] (loop [[line & more-lines] input-lines
                                  cells #{}]
                             (if (str/blank? line)
                               [cells more-lines]
                               (recur more-lines
                                      (conj cells
                                            (->> (str/split line #",")
                                                 (mapv util/parse-long))))))
        folds (vec
               (for [line more-lines]
                 (let [[_ dim dim-v] (re-matches #"fold along (\w+)=(\d+)" line)]
                   [(keyword dim) (util/parse-long dim-v)])))]
    {:cells cells, :folds folds}))

(def example-input
  (parse-input (str/split-lines (str/trim "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))))

(let [dim-v 7
      x 7]
  (- (* dim-v 2) x))

(defn fold-paper [cells [dim dim-v]]
  (->> cells
       (into #{} (map (fn [[x y]]
                        [(cond->> x
                           (and (= dim :x) (> x dim-v)) (- (* 2 dim-v)))
                         (cond->> y
                           (and (= dim :y) (> y dim-v)) (- (* 2 dim-v)))])))))

(defn p1 [{:keys [cells folds]}]
  (count (fold-paper cells (first folds))))

(t/deftest test-p1
  (t/is (= 17 (p1 example-input)))
  (t/is (= 701 (util/with-line-seq "day13.txt" (comp p1 parse-input)))))

(defn render-cells [cells]
  (let [lines (group-by second cells)]
    (doseq [line-idx (range (inc (apply max (keys lines))))
            :let [line (get lines line-idx)
                  cols (set (map first line))]]
      (println (if (seq cols)
                 (->> (for [col-idx (range (inc (apply max cols)))]
                        (if (cols col-idx) \X \space))
                      str/join)
                 "")))))

(defn p2 [{:keys [cells folds]}]
  (render-cells (reduce fold-paper cells folds)))

(comment
  (p2 example-input)
  (util/with-line-seq "day13.txt" (comp p2 parse-input)))
