(ns aoc2021.day12
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [aoc2021.util :as util]))

(defn parse-input [lines]
  (->> lines
       (reduce (fn [conns line]
                 (let [[from to] (str/split line #"-")]
                   (-> conns
                       (update from (fnil conj #{}) to)
                       (update to (fnil conj #{}) from))))
               {})))

(def example-1
  (parse-input (str/split-lines (str/trim "
start-A
start-b
A-c
A-b
b-d
A-end
b-end"))))

(def example-2
  (parse-input (str/split-lines (str/trim "
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"))))

(defn paths [init-q ->more-q]
  (loop [[{:keys [path cur] :as q} & more-q] init-q
         paths #{}]
    (cond
      (nil? q) paths
      (= "end" cur) (recur more-q (conj paths path))
      :else (recur (into more-q (->more-q q)) paths))))

(defn p1 [conns]
  (-> (paths [{:path ["start"], :small-caves #{"start"}, :cur "start"}]
             (fn [{:keys [path small-caves cur]}]
               (for [to (get conns cur)
                     :let [small-cave? (Character/isLowerCase (first to))]
                     :when (or (not small-cave?)
                               (not (contains? small-caves to)))]
                 {:path (conj path to)
                  :small-caves (cond-> small-caves small-cave? (conj to))
                  :cur to})))
      count))

(t/deftest test-p1
  (t/is (= 10 (p1 example-1)))
  (t/is (= 19 (p1 example-2)))
  (t/is (= 4378 (util/with-line-seq "day12.txt" (comp p1 parse-input)))))

(defn p2 [conns]
  (-> (paths [{:path ["start"], :small-caves #{"start"}, :double-yet? false, :cur "start"}]
             (fn [{:keys [path small-caves cur double-yet?]}]
               (for [to (get conns cur)
                     :let [small-cave? (Character/isLowerCase (first to))]
                     :when (or (not small-cave?)
                               (not (contains? small-caves to))
                               (and (not double-yet?) (not= "start" to)))]
                 {:path (conj path to)
                  :small-caves (cond-> small-caves small-cave? (conj to))
                  :double-yet? (or double-yet? (contains? small-caves to))
                  :cur to})))
      count))

(t/deftest test-p2
  (t/is (= 36 (p2 example-1)))
  (t/is (= 103 (p2 example-2)))
  (t/is (= 133621 (util/with-line-seq "day12.txt" (comp p2 parse-input)))))
