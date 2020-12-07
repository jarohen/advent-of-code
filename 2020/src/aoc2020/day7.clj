(ns aoc2020.day7
  (:require [aoc2020.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]
            [sss.arrows :refer [->% ->>%]]
            [clojure.test :as t]))

(defn parse-bags [lines]
  (->> (for [line lines]
         (let [[_ bag-color contents] (re-matches #"(.+?) bags contain (.+?)" line)]
           [bag-color (->> (for [part (str/split contents #", ")]
                             (when-let [[_ x content-color] (re-matches #"(\d+) (.+?) bags?\.?" part)]
                               [content-color (Long/parseLong x)]))
                           (into {}))]))
       (into {})))

(defn eventually-contains-gold [bags]
  (let [inverted (->> (for [[outer-bag contents] bags
                            inner-bag (keys contents)]
                        [outer-bag inner-bag])
                      (group-by second)
                      (into {} (map (juxt key (->>% val (into #{} (map first)))))))]
    (loop [[q-bag & more-bags] (get inverted "shiny gold")
           res #{}]
      (if-not q-bag
        res
        (recur (concat more-bags (get inverted q-bag)) (conj res q-bag))))))

(def p1-test-case
  (str/trim "
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"))

(t/deftest test-p1
  (t/is (= 4 (->> (str/split-lines p1-test-case)
                  parse-bags
                  eventually-contains-gold
                  count)))
  (t/is (= 248 (util/with-line-seq "day7.txt"
                 (->>% parse-bags
                       eventually-contains-gold
                       count)))))

(def p2-test-case
  (str/trim "
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."))

(defn contained-bags [bags]
  (loop [state bags]
    (or (let [x (get state "shiny gold")]
          (when (number? x)
            x))
        (recur (->> (for [[outer-color contents] state]
                      [outer-color (if (number? contents)
                                     contents
                                     (let [nxt (for [[inner-color bag-count] contents]
                                                 (let [inner-count (get state inner-color)]
                                                   (when (number? inner-count)
                                                     (* bag-count (inc inner-count)))))]
                                       (if (every? number? nxt)
                                         (reduce + nxt)
                                         contents)))])
                    (into {}))))))

(t/deftest test-p2
  (t/is (= 126 (contained-bags (parse-bags (str/split-lines p2-test-case)))))
  (t/is (= 57281 (util/with-line-seq "day7.txt" (->>% parse-bags contained-bags)))))
