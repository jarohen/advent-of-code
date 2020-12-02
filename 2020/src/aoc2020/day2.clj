(ns aoc2020.day2
  (:require [aoc2020.util :as util]))

(defn part1 [lines]
  (->> lines
       (filter (fn [line]
                 (let [[_ mn mx [letter] pw] (re-matches #"(\d+)-(\d+) (.): (.*)" line)]
                   (<= (Long/parseLong mn)
                       (get (frequencies pw) letter 0)
                       (Long/parseLong mx)))))
       (count)))

(defn part2 [lines]
  (->> lines
       (filter (fn [line]
                 (let [[_ idx1 idx2 [letter] pw] (re-matches #"(\d+)-(\d+) (.): (.*)" line)]
                   (not= (= (get pw (dec (Long/parseLong idx1))) letter)
                         (= (get pw (dec (Long/parseLong idx2))) letter)))))
       (count)))

(comment
  (util/with-line-seq "day2.txt"
    (fn [lines]
      [(part1 lines)
       (part2 lines)])))
