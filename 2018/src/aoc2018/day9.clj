(ns aoc2018.day9
  (:require [clojure.test :as t]
            [sss.arrows :refer [->%]])
  (:import [java.util LinkedList]))

(defn play-game [{:keys [player-count max-marble]}]
  (let [marbles (LinkedList. [0])]
    (loop [idx 0
           next-player 0
           next-marble 1
           scores {}]

      (cond
        (> next-marble max-marble) scores

        (zero? (mod next-marble 23))
        (let [remove-idx (mod (- idx 7) (count marbles))
              removed-marble (.remove marbles (int remove-idx))]

          (recur remove-idx
                 (mod (inc next-player) player-count)
                 (inc next-marble)
                 (update scores next-player (fnil + 0) next-marble removed-marble)))

        :else
        (let [insert-idx (inc (mod (inc idx) (count marbles)))]
          (.add marbles insert-idx next-marble)
          (recur insert-idx
                 (mod (inc next-player) player-count)
                 (inc next-marble)
                 scores))))))

(defn ->max-score [scores]
  (->> scores vals (apply max)))

(comment
  (time
   (-> (play-game {:player-count 405, :max-marble 7095300})
       ->max-score)))

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
    ))
