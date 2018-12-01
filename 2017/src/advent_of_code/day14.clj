(ns advent-of-code.day14
  (:require [advent-of-code.day10 :as d10]
            [clojure.set :as set]
            [clojure.string :as s]))

(def input
  "vbqugkhl")

(def demo-input
  "flqrgnkx")

(defn ->binary-string [x]
  (format "%08d" (Long/parseLong (Long/toBinaryString x))))

(defn grid [input]
  (for [key-string (map #(str input "-" %) (range 128))]
    (->> key-string
         d10/knot-hash
         (map ->binary-string)
         s/join)))

(defn p1 [input]
  (count (filter #{\1} (s/join (grid input)))))

(defn neighbour-idxs [idx row-size]
  (let [pos [(quot idx row-size) (rem idx row-size)]]
    (->> (map #(mapv + pos %) [[1 0] [0 1] [-1 0] [0 -1]])
         (into #{} (comp (filter (fn [neighbour]
                                   (every? #(<= 0 % (dec row-size)) neighbour)))
                         (map (fn [[x y]]
                                (+ (* x row-size) y))))))))

(defn clusters [grid]
  (let [row-size (count (first grid))
        flat-grid (into [] (mapcat identity) grid)]
    (loop [idx 0
           clusters #{}
           visited #{}]
      (cond
        (= idx (count flat-grid)) clusters

        (contains? visited idx) (recur (inc idx) clusters visited)

        :else (case (nth flat-grid idx)
                \0 (recur (inc idx) clusters visited)
                \1 (let [cluster (loop [[neighbour & more] [idx]
                                        cluster #{}]
                                   (if-not neighbour
                                     cluster
                                     (case (nth flat-grid neighbour)
                                       \0 (recur more cluster)
                                       \1 (let [new-neighbours (set/difference (into #{} (neighbour-idxs neighbour row-size)) cluster)]
                                            (recur (concat new-neighbours (remove new-neighbours more)) (conj cluster neighbour))))))]

                     (recur (inc idx) (conj clusters cluster) (set/union visited cluster))))))))

(defn p2 [input]
  (count (clusters (grid input))))
