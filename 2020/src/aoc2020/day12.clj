(ns aoc2020.day12
  (:require [aoc2020.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]))

(def test-case
  ["F10"
   "N3"
   "F7"
   "R90"
   "F11"])

(defn parse-line [line]
  (let [[_ op arg] (re-matches #"(.)(\d+)" line)
        arg (util/parse-long arg)]
    (case op
      "N" [:move [0 (- arg)]]
      "S" [:move [0 arg]]
      "E" [:move [arg 0]]
      "W" [:move [(- arg) 0]]
      "L" [:rotate (case arg
                     90 [0 1, -1 0]
                     180 [-1 0, 0 -1]
                     270 [0 -1, 1 0])]
      "R" [:rotate (case arg
                     90 [0 -1, 1 0]
                     180 [-1 0, 0 -1]
                     270 [0 1, -1 0])]
      "F" [:forward arg])))

(defn rotate [[x y] [*xx *xy *yx *yy]]
  [(+ (* x *xx) (* y *xy))
   (+ (* x *yx) (* y *yy))])

(defn apply-p1 [state [op arg]]
  (case op
    :move (-> state (update :loc (->>% (mapv + arg))))
    :rotate (-> state (update :dir rotate arg))
    :forward (-> state (update :loc (->>% (mapv + (mapv * [arg arg] (:dir state))))))))

(defn apply-p2 [state [op arg]]
  (case op
    :move (-> state (update :wp (->>% (mapv + arg))))
    :rotate (-> state (update :wp rotate arg))
    :forward (-> state (update :loc (->>% (mapv + (mapv * [arg arg] (:wp state))))))))

(defn d12 [f init lines]
  (->> lines
       (map parse-line)
       (reduce f init)
       :loc
       (reduce +)))

(def p1
  (->>% (d12 apply-p1 {:loc [0 0], :dir [1 0]})))

(def p2
  (->>% (d12 apply-p2 {:loc [0 0], :wp [10 -1]})))

(t/deftest test-d12
  (t/is (= 25 (p1 test-case)))
  (t/is (= 286 (p2 test-case)))
  (t/is (= [2057 71504]
           (util/with-line-seq "day12.txt" (juxt p1 p2)))))
