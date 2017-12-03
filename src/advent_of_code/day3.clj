(ns advent-of-code.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn pt1 [x]
  (let [sqrt (long (Math/sqrt (float x)))
        odd-sqrt (cond-> sqrt
                   (even? sqrt) dec)
        ring (/ (inc odd-sqrt) 2)
        offset (- x (* odd-sqrt odd-sqrt))
        segment (quot offset ring)
        segment-offset (mod offset (* segment ring))]
    (+ ring (if (even? segment)
              (- ring segment-offset)
              segment-offset))))
