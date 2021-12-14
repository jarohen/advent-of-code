(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [aoc2021.util :as util])
  (:import clojure.lang.MapEntry))

(defn parse-input [input-lines]
  (let [[start _ & rules] input-lines]
    {:start (vec start)
     :rules (->> rules
                 (into {} (map (fn [rule]
                                 (let [[search replace] (str/split rule #" -> ")]
                                   (MapEntry/create (seq search) (first replace)))))))}))

(def example-data
  (parse-input (str/split-lines (str/trim "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))))

(defn rules-seq [{:keys [start rules]}]
  (iterate (fn [acc]
             (reduce (fn [acc [[h t :as pair] freq]]
                       (let [m (get rules pair)]
                         (-> acc
                             (update [h m] (fnil + 0) freq)
                             (update [m t] (fnil + 0) freq))))
                     {}
                     acc))
           (frequencies (partition 2 1 start))))

(defn puzzle [n {:keys [start] :as input}]
  (->> (nth (rules-seq input) n)
       (map (juxt (comp first key) val)) ; heads
       (reduce (fn [acc [h freq]]
                 (update acc h (fnil + 0) freq))
               {(last start) 1})
       vals
       sort
       ((juxt last first))
       (apply -)))

(def p1 (partial puzzle 10))
(def p2 (partial puzzle 40))

(t/deftest test-puzzle
  (t/is (= 1588 (p1 example-data)))
  (t/is (= 3213 (util/with-line-seq "day14.txt" (comp p1 parse-input))))

  (t/is (= 2188189693529 (p2 example-data)))
  (t/is (= 3711743744429 (util/with-line-seq "day14.txt" (comp p2 parse-input)))))
