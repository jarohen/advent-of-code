(ns aoc2020.day8
  (:require [aoc2020.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->>%]]))

(defn parse-prog [lines]
  (zipmap (range)
          (for [line lines]
            (let [[_ op arg] (re-matches #"(.+?) ([+-]\d+)" line)]
              [(keyword op) (Long/parseLong arg)]))))

(defn run-prog [prog]
  (loop [pc 0
         acc 0
         seen (transient #{})]
    (cond
      (contains? seen pc) [:loop acc]
      (>= pc (count prog)) [:term acc]
      :else (let [[op arg] (get prog pc)
                  seen (conj! seen pc)]
              (case op
                :nop (recur (inc pc) acc (conj! seen pc))
                :acc (recur (inc pc) (+ acc arg) (conj! seen pc))
                :jmp (recur (+ pc arg) acc (conj! seen pc)))))))

(defn perms [prog]
  (for [[idx [op arg]] prog
        :let [flip ({:nop :jmp, :jmp :nop} op)]
        :when flip]
    (assoc prog idx [flip arg])))

(t/deftest test-d8
  (t/is (= [:loop 1337]
           (util/with-line-seq "day8.txt"
             (->>% parse-prog run-prog))))

  (t/is (= [:term 1358]
           (util/with-line-seq "day8.txt"
             (->>% parse-prog
                   perms
                   (map run-prog)
                   (filter (comp #{:term} first))
                   first)))))
