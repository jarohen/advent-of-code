(ns aoc2018.day3
  (:require [aoc2018.util :as util]
            [clojure.set :as set]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]))

(defn parse-file [lines]
  (for [line lines]
    (-> (zipmap [:id :x :y :width :height]
                (->> (rest (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line))
                     (map util/parse-long))))))

(defn ->squares [{:keys [id x y width height]}]
  (for [x (range x (+ x width))
        y (range y (+ y height))]
    {:id id
     :sq [x y]}))

(defn overlapping-squares [squares]
  (->> (frequencies (map :sq squares))
       (into #{} (comp (filter (->% val (> 1)))
                       (map key)))))

(defn p1 [lines]
  (->> lines
       parse-file
       (mapcat ->squares)
       overlapping-squares
       count))

(defn sq-dupes [sqs]
  (->> sqs
       (group-by :sq)
       (into #{} (comp (filter (->% val count (> 1)))
                       (map val)
                       (mapcat (->>% (map :id)))))))

(defn p2 [lines]
  (let [parsed-lines (parse-file lines)]
    (set/difference (into #{} (map :id) parsed-lines)
                    (->> parsed-lines
                         (mapcat ->squares)
                         sq-dupes))))

(t/deftest test-d3
  (let [sample ["#1 @ 1,3: 4x4"
                "#2 @ 3,1: 4x4"
                "#3 @ 5,5: 2x2"]]
    (t/is (= 4 (p1 sample)))
    (t/is (= #{3} (p2 sample)))))

(comment
  (util/with-line-seq "day3.txt" p1)
  (util/with-line-seq "day3.txt" p2))
