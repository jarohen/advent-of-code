(ns advent-of-code.day9-test
  (:require [advent-of-code.day9 :as sut]
            [clojure.test :as t]))

(t/deftest testing-pt1
  (t/are [in score] (= (first (sut/d9 in)) score)
    "{}" 1
    "{{{}}}" 6
    "{{},{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))
