(ns advent-of-code.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-grid [grid-str]
  (let [cells (filter #{\# \.} (str/trim grid-str))
        grid-size (int (Math/sqrt (count cells)))
        offset (quot grid-size 2)]
    (->> cells
         (into {} (map-indexed (fn [idx cell]
                                 [[(- (rem idx grid-size) offset)
                                   (- offset (quot idx grid-size))]
                                  (if (= cell \#)
                                    :infected
                                    :clean)]))))))

(def grid
  (parse-grid (slurp (io/resource "day22.txt"))))

(def turns
  {[0 1] {:left [-1 0], :right [1 0], :reverse [0 -1]}
   [-1 0] {:left [0 -1], :right [0 1], :reverse [1 0]}
   [0 -1] {:left [1 0], :right [-1 0], :reverse [0 1]}
   [1 0] {:left [0 1], :right [0 -1], :reverse [-1 0]}})

(defn play [grid {:keys [burst-count nxt]}]
  (loop [grid grid
         pos [0 0]
         dir [0 1]
         infection-count 0
         bursts 0]
    (if (= bursts burst-count)
      infection-count
      (let [{:keys [state turn]} (nxt (get grid pos :clean))
            grid (assoc grid pos state)
            dir (get-in turns [dir turn] dir)
            pos (mapv + pos dir)]
        (recur grid pos dir (cond-> infection-count (= :infected state) inc) (inc bursts))))))

(defn p1 [grid]
  (play grid {:nxt {:clean {:state :infected, :turn :left}
                    :infected {:state :clean, :turn :right}}
              :burst-count 10000}))

(defn p2 [grid]
  (play grid {:nxt {:clean {:state :weakened, :turn :left}
                    :weakened {:state :infected, :turn :straight}
                    :infected {:state :flagged, :turn :right}
                    :flagged {:state :clean, :turn :reverse}}
              :burst-count 10000000}))
