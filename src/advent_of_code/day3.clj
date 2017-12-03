(ns advent-of-code.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn pt1 [x]
  (let [sqrt (long (Math/sqrt (float (dec x))))
        odd-sqrt (cond-> sqrt
                   (even? sqrt) dec)
        ring (/ (inc odd-sqrt) 2) ; how many rings its out from the centre
        offset (- x (* odd-sqrt odd-sqrt)) ; its offset within the ring
        segment (quot offset ring) ; which segment within the ring - split the segment into 9
        segment-offset (mod offset (* segment ring)) ; our offset within the segment
        ]
    (+ ring (if (even? segment)
              (- ring segment-offset)
              segment-offset))))
