(ns aoc2018.day12
  (:require [aoc2018.util :as util]
            [sss.arrows :refer [->% ->>%]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn plant? [c] (= c \#))

(defn parse-rules [rule-lines]
  (->> rule-lines
       (into {} (map (fn [line]
                       (let [[pre post] (rest (re-matches #"([.#]{5}) => ([.#])" line))]
                         [(mapv plant? pre) (plant? (first post))]))))))

(defn parse-initial-state [line]
  (into (sorted-set)
        (keep-indexed (fn [idx c] (when (plant? c) idx)))
        line))

(defn step [plants {:keys [rules]}]
  (->> (range (- (first plants) 4)
              (+ (first (rseq plants)) 4))
       (partition 5 1)
       (keep (fn [idxes]
               (when (rules (map (comp boolean plants) idxes))
                 (nth idxes 2))))
       (apply sorted-set)))

(defn states [{:keys [initial-state rules n]}]
  (iterate (->% (step {:rules (some-fn rules (constantly false))}))
           initial-state))

(t/deftest test-day12
  (let [foo-initial-state (parse-initial-state "#..#.#..##......###...###")
        foo-rules (parse-rules ["...## => #"
                                "..#.. => #"
                                ".#... => #"
                                ".#.#. => #"
                                ".#.## => #"
                                ".##.. => #"
                                ".#### => #"
                                "#.#.# => #"
                                "#.### => #"
                                "##.#. => #"
                                "##.## => #"
                                "###.. => #"
                                "###.# => #"
                                "####. => #"])]
    (t/is (= 325
             (-> (states {:initial-state foo-initial-state
                          :rules foo-rules})
                 (nth 20)
                 (->> (reduce +)))))))

(comment
  (-> (states {:initial-state (parse-initial-state (slurp (io/resource "day12-initial.txt")))
               :rules (util/with-line-seq "day12-rules.txt" parse-rules)})
      (nth 20)
      (->> (reduce +)))

  ;; part 2 repeats after 2000 (probably before, but I found it at 2000)
  (->> (states {:initial-state (parse-initial-state (slurp (io/resource "day12-initial.txt")))
                :rules (util/with-line-seq "day12-rules.txt" parse-rules)})
       (drop 2000)
       (take 10)
       (map (->>% (reduce +))))

  (+ (* (- (long 50e9) 2000)
        38)
     76384))
