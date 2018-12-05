(ns aoc2018.day5
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [sss.arrows :refer [->% ->>%]]))

(defn eliminates? [a b]
  (and (not= a b)
       (= (Character/toLowerCase a)
          (Character/toLowerCase b))))

(defn eliminate [s]
  (time
   (loop [[b & before] '()
          [a & after] s]
     (cond
       (nil? a) (count (cons b before))
       (nil? b) (recur (list a) after)
       (eliminates? b a) (recur before after)
       :else (recur (cons a (cons b before)) after)))))

(def char-pairs
  (mapv hash-set
        (map #(char (+ (int \a) %)) (range 26))
        (map #(char (+ (int \A) %)) (range 26))))

(defn remove-bad-polymer [s]
  (->> (pmap (fn [char-pair]
               (->> (remove char-pair s) eliminate))
             char-pairs)
       (apply min)))

(t/deftest test-polymers
  (let [sample "dabAcCaCBAcCcaDA"]
    (t/is (= 10 (eliminate sample)))
    (t/is (= 4 (remove-bad-polymer sample)))))

(comment
  (let [s (str/trim (slurp (io/resource "day5.txt")))]
    [(eliminate s)
     (remove-bad-polymer s)]))
