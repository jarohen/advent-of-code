(ns aoc2018.day10
  (:require [aoc2018.util :as util]
            [clojure.java.io :as io]
            [sss.arrows :refer [->% ->>%]])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(defn parse-input [lines]
  (->> lines
       (map (->>% (re-matches #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")
                  rest
                  (map util/parse-long)
                  (zipmap [:x :y :dx :dy])))))

(defn step [{:keys [x y dx dy]}]
  {:x (+ x dx)
   :y (+ y dy)
   :dx dx
   :dy dy})

(defn write-image! [{:keys [points file]}]
  (let [min-x (apply min (map :x points))
        max-x (apply max (map :x points))
        min-y (apply min (map :y points))
        max-y (apply max (map :y points))

        img (BufferedImage. (inc (- max-x min-x))
                            (inc (- max-y min-y))
                            BufferedImage/TYPE_INT_RGB) ]

    (doseq [{:keys [x y]} points]
      (.setRGB img (- x min-x) (- y min-y) (.getRGB Color/WHITE)))

    (ImageIO/write img "png" file)))

(comment
  (util/with-line-seq "day10.txt"
    (fn [lines]
      (->> (iterate #(mapv step %) (parse-input lines))
           (drop 10300)
           (take 100)
           (map-indexed (fn [idx points]
                          (write-image! {:file (io/file (format "/tmp/out%02d.png" idx))
                                         :points points})))
           dorun))))
