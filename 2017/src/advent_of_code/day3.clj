(ns advent-of-code.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def turn-left
  {[1 0] [0 1]
   [0 1] [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]})

(defn make-spiral [f]
  (letfn [(spiral* [{:keys [loc dir n spiral] :as here}]
            (lazy-seq
              (let [left-dir (turn-left dir)
                    next-dir (if (get spiral (mapv + left-dir loc))
                               dir
                               left-dir)
                    next-loc (mapv + next-dir loc)]
                (cons here (spiral* {:loc next-loc
                                     :dir next-dir
                                     :n (inc n)
                                     :spiral (f spiral
                                                {:loc next-loc
                                                 :n (inc n)})})))))]

    (spiral* {:loc [0 0]
              :dir [1 0]
              :n 1
              :spiral {[0 0] 1}})))

(defn distance [loc]
  (->> loc
       (map #(Math/abs %))
       (apply +)))

(defn pt1 [n]
  (-> (make-spiral (fn [spiral {:keys [n loc]}]
                     (assoc spiral loc n)))
      (nth (dec n))
      :loc
      distance))

(def surrounding-locs
  (vec (for [x [-1 0 1]
             y [-1 0 1]
             :when (not (and (zero? x) (zero? y)))]
         [x y])))

(defn pt2 [n]
  (->> (make-spiral (fn [spiral {:keys [n loc]}]
                      (assoc spiral loc (->> (keep (fn [surrounding-loc]
                                                     (get spiral (mapv + loc surrounding-loc)))
                                                   surrounding-locs)
                                             (apply +')))))
       (keep (fn [{:keys [spiral loc]}]
               (when (> (get spiral loc) n)
                 (get spiral loc))))
       first))
