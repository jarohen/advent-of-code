(ns aoc2020.day1
  (:require [aoc2020.util :as util]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]))

(defn with-input [f]
  (util/with-line-seq "day1.txt" (->>% (map #(Long/parseLong %)) f)))

(defn day-1 []
  (with-input
    (fn [lines]
      [(first (for [x lines
                    y lines
                    :when (= 2020 (+ x y))]
                (* x y)))
       (first (for [x lines
                    y lines
                    z lines
                    :when (= 2020 (+ x y z))]
                (* x y z)))])))
