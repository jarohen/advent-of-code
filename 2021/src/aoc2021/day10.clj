(ns aoc2021.day10
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [aoc2021.util :as util]))

(def example-data (str/split-lines (str/trim "
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")))

(def matches
  {\( \), \{ \}, \[ \], \< \>})

(defn check
  "return either the remaining stack, or the first mismatch character"
  [line]
  (->> line
       (reduce (fn [[head & tail :as stack] ch]
                 (if (= ch head)
                   tail
                   (if-let [match (get matches ch)]
                     (cons match stack)
                     (reduced ch))))
               '())))

(def p1-points {\) 3, \] 57, \} 1197, \> 25137})

(defn p1 [lines]
  (->> lines (map check) (remove seq?) (map p1-points) (apply +)))

(t/deftest test-p1
  (t/is (= 26397 (p1 example-data)))
  (t/is (= 392139 (util/with-line-seq "day10.txt" p1))))

(def p2-points {\) 1, \] 2, \} 3, \> 4})

(defn p2 [lines]
  (->> lines (map check) (filter seq?)
       (map (fn [stack]
              (->> stack
                   (reduce (fn [acc el]
                             (+ (* 5 acc) (p2-points el)))
                           0))))
       (sort)
       (#(nth % (/ (count %) 2)))))

(t/deftest test-p2
  (t/is (= 288957 (p2 example-data)))
  (t/is (= 4001832844 (util/with-line-seq "day10.txt" p2))))
