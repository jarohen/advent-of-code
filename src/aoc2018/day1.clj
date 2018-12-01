(ns aoc2018.day1
  (:require [aoc2018.files :as files]
            [clojure.java.io :as io]
            [sss.arrows :refer [->% ->>%]]))

(defn with-input [f]
  (files/with-line-seq "day1.txt" (->>% (map #(Long/parseLong %)) f)))

(defn part-a [nums]
  (reduce + nums))

(defn part-b [nums]
  (->> (cycle nums)
       (reductions +)
       (reduce (fn [acc el]
                 (if (contains? acc el)
                   (reduced el)
                   (conj acc el)))
               #{0})))

(comment
  (with-input (->>% part-b)))
