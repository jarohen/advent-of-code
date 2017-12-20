(ns advent-of-code.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [instaparse.core :as i]))

(i/defparser parser
  "<Particle> = pos <', '> vel <', '> acc
   pos = <'p='> three-d
   vel = <'v='> three-d
   acc = <'a='> three-d
   <three-d> = <'<'> num <','> num <','> num <'>'>
   num = #'-?\\d+'")

(defn parse-line [idx line]
  (let [[[_ & pos] [_ & vel] [_ & acc]] (i/transform {:num #(Long/parseLong %)}
                                                     (parser line))]
    {:idx idx, :p (vec pos), :v (vec vel), :a (vec acc)}))

(def input
  (->> (slurp (io/resource "day20.txt"))
       s/split-lines
       (map-indexed parse-line)))

(defn manhattan [[x y z]]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn p1 [input]
  (:idx (first (sort-by (comp manhattan :a) input))))

(defn tick [particles]
  (->> (for [{:keys [idx p v a]} particles]
         (let [v (mapv + v a)]
           {:idx idx
            :p (mapv + p v)
            :v v
            :a a}))
       (group-by :p)
       vals
       (into #{} (comp (filter (comp #(= 1 (count %))))
                       (mapcat identity)))
       (sort-by :idx)))

(defn p2 [input]
  (->> (iterate tick input)
       ;; it _seems_ to stabilise after ~40-50 iterations
       ;; I'm sure there's a way to actually calculate the fix point, though
       (take 50)
       (map count)
       distinct))
