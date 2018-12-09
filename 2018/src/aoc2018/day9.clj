(ns aoc2018.day9
  (:require [clojure.test :as t]
            [sss.arrows :refer [->%]])
  (:import [java.util LinkedList]))

(defn play-game [{:keys [player-count max-marble]}]
  (let [marbles (LinkedList. [0])]
    (loop [next-player 0
           next-marble 1
           scores {}]

      (cond
        (> next-marble max-marble) scores

        (zero? (mod next-marble 23))
        (do
          (dotimes [n 7]
            (.add marbles 0 (.removeLast marbles)))

          (let [removed-marble (.removeLast marbles)]
            (.add marbles (.removeFirst marbles))
            (recur (mod (inc next-player) player-count)
                   (inc next-marble)
                   (update scores next-player (fnil + 0) next-marble removed-marble))))

        :else
        (do
          (.add marbles (.removeFirst marbles))
          (.add marbles next-marble)
          (recur (mod (inc next-player) player-count)
                 (inc next-marble)
                 scores))))))

(defn ->max-score [scores]
  (->> scores vals (apply max)))

(t/deftest test-max-score
  (t/are [player-count max-marble max-score] (= max-score (-> (play-game {:player-count player-count,
                                                                          :max-marble max-marble})
                                                              ->max-score))
    9 25 32

    10 1618 8317
    13 7999 146373
    17 1104 2764
    21 6111 54718
    30 5807 37305

    405 70953 422980

    405 7095300 3552041936))
