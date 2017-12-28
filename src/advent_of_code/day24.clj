(ns advent-of-code.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def components
  (for [line (str/split-lines (slurp (io/resource "day24.txt")))]
    (let [[p1 p2] (str/split line #"/")]
      [(Long/parseLong p1) (Long/parseLong p2)])))

(defn remove-one [coll x]
  (let [[before after] (split-with (complement #{x}) coll)]
    (vec (concat before (rest after)))))

(defn remove-component [cmap [p1 p2]]
  (-> cmap
      (update p1 remove-one p2)
      (update p2 remove-one p1)))

(defn all-bridges [components]
  (let [cmap (reduce (fn [acc [p1 p2]]
                       (-> acc
                           (update p1 (fnil conj []) p2)
                           (update p2 (fnil conj []) p1)))
                     {}
                     components)]
    (loop [[{:keys [cmap bridge cur] :as q} & more-q] (for [p2 (get cmap 0)]
                                                        {:cmap (-> cmap (remove-component [0 p2]))
                                                         :bridge [[0 p2]]
                                                         :cur p2})
           bridges #{}]
      (if (nil? q)
        bridges
        (recur (into more-q (for [nxt (get cmap cur)]
                              {:cmap (-> cmap (remove-component [cur nxt]))
                               :bridge (conj bridge [cur nxt])
                               :cur nxt}))
               (cond-> bridges (empty? (get cmap cur)) (conj bridge)))))))

(defn strength [bridge]
  (transduce (mapcat identity) + bridge))

(defn p1 [components]
  (->> (map strength (all-bridges component))
       sort
       last))

(defn p2 [components]
  (->> (map (juxt count strength) (all-bridges components))
       sort
       last))
