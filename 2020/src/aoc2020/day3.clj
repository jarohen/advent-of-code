(ns aoc2020.day3
  (:require [aoc2020.util :as util]
            [sss.arrows :refer [->%]]))

(defn day3 [lines moves]
  (->> (reduce (fn [acc [idx line]]
                 (mapv + acc
                       (for [[dx dy] moves]
                         (or (if (and (zero? (mod idx dy))
                                      (= \# (nth line (mod (* dx (/ idx dy))
                                                           (count line)))))
                               1
                               0)))))

               (repeat (count moves) 0)
               (map vector (range) lines))
       (reduce *)))

(comment
  (util/with-line-seq "day3.txt" (->% (day3 [[3 1]])))
  (util/with-line-seq "day3.txt" (->% (day3 [[1 1] [3 1] [5 1] [7 1] [1 2]]))))
