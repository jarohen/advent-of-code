(ns advent-of-code.day11
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def input
  (-> (s/trim (slurp (io/resource "day11.txt")))
      (s/split #",")
      (->> (map keyword))))

(def dirs
  {:n [0 1]
   :s [0 -1]
   :nw [-0.5 0.5]
   :ne [0.5 0.5]
   :sw [-0.5 -0.5]
   :se [0.5 -0.5]})

(defn positions [steps]
  (reductions #(mapv + %1 %2) [0 0] (map dirs steps)))

(defn distance [[x y]]
  (let [lower (min (Math/abs x) (Math/abs y))
        higher (max (Math/abs x) (Math/abs y))]
    (int (+ (* 2 lower) ; go NE/NW/SE/SW until you end up directly above/below
            ;; go N/S for the remainder
            (- higher lower)))))

(defn p1 [input]
  (distance (last (positions input))))

(defn p2 [input]
  (apply max (map distance (positions input))))
