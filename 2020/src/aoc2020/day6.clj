(ns aoc2020.day6
  (:require [aoc2020.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]
            [sss.arrows :refer [->%]]))

(def p1-test-case
  (str/trim "
abc

a
b
c

ab
ac

a
a
a
a

b"))

(defn d6 [lines combine]
  (->> (for [group (->> lines
                        (partition-by str/blank?))
             :when (not= [""] group)]

         (->> (for [person group]
                (into #{} person))
              (apply combine)
              count))
       (reduce +)))

(comment
  (util/with-line-seq "day6.txt" (->% (d6 set/union)))
  (util/with-line-seq "day6.txt" (->% (d6 set/intersection))))
