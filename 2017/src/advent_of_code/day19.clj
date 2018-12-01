(ns advent-of-code.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (slurp (io/resource "day19.txt"))
       s/split-lines
       (mapv vec)))

(defn start [input]
  [0 (->> (map vector (range) (first input))
          (remove (comp #{\space} second))
          ffirst)])

(defn side-dirs [[x y]]
  (if (zero? x)
    #{[1 0] [-1 0]}
    #{[0 1] [0 -1]}))

(defn road? [n]
  (not (or (nil? n) (= n \space))))

(defn d19 [input]
  (loop [pos (start input)
         dir [1 0]
         steps []]
    (let [pos-ahead (mapv + pos dir)
          road-ahead? (road? (get-in input pos-ahead))
          side-roads (->> (side-dirs dir)
                          (into #{} (comp (map (fn [side-dir]
                                                 {:dir side-dir
                                                  :pos (mapv + pos side-dir)}))
                                          (filter (comp road? #(get-in input %) :pos)))))
          steps (conj steps (get-in input pos))]
      (cond
        (and (not road-ahead?) (empty? side-roads)) [(->> (remove #{\| \+ \-} steps)
                                                          s/join)
                                                     (count steps)]
        road-ahead? (recur pos-ahead dir steps)
        :else (let [{:keys [pos dir]} (first (seq side-roads))]
                (recur pos dir steps))))))
