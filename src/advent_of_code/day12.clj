(ns advent-of-code.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as s]))

(def input
  (->> (s/split-lines (slurp (io/resource "day12.txt")))
       (into {}
             (map (fn [line ]
                    (let [[node children] (s/split line #" <-> ")]
                      [(Long/parseLong node) (->> (s/split children #", ")
                                                  (into #{} (map #(Long/parseLong %))))]))))))

(defn sub-graph [{:keys [graph node]}]
  (loop [[node & more-nodes] [node]
         res #{}]
    (cond
      (nil? node) res
      (contains? res node) (recur more-nodes res)

      :else (let [children (get graph node)]
              (recur (concat more-nodes (remove res children)) (conj res node))))))

(defn p1 [input]
  (sub-graph {:graph input, :node 0}))

(defn sub-graphs [graph]
  (loop [[node & more-nodes] (keys graph)
         res #{}]
    (cond
      (nil? node) res
      :else (let [sg (sub-graph {:graph graph, :node node})]
              (recur (remove sg more-nodes) (conj res sg))))))

(defn p2 [input]
  (count (sub-graphs input)))
