(ns aoc2021.day11
  (:require [aoc2021.util :as util]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn parse-input [input]
  (->> (str/split-lines input)
       str/join
       (mapv #(Character/digit % 10))))

(def example-data
  (->> (str/trim "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")
       parse-input))

(def puzzle-input (parse-input (str/trim (slurp (io/resource "day11.txt")))))

(def neighbours (util/->neighbours 10 10 {:diags? true}))

(defn step [energies]
  (loop [energies (transient energies)
         flashes #{}
         q (range 100)]
    (let [{:keys [energies new-flashes]}
          (reduce (fn [{:keys [energies new-flashes] :as acc} idx]
                    (let [v (nth energies idx)]
                      (cond
                        (contains? flashes idx) acc
                        (contains? new-flashes idx) acc
                        (= v 9) {:energies (assoc! energies idx 0)
                                 :new-flashes (conj new-flashes idx)}
                        :else (update acc :energies assoc! idx (inc v)))))
                  {:energies energies
                   :new-flashes #{}}
                  q)]
      (if (empty? new-flashes)
        {:energies (persistent! energies), :flashes (count flashes)}
        (recur energies
               (into flashes new-flashes)
               (into [] (mapcat neighbours) new-flashes))))))

(defn p1 [energies]
  (->> (iterate (fn [{:keys [energies]}]
                  (step energies))
                {:energies energies, :flashes 0})
       (take 101)
       (map :flashes)
       (apply +)))

(t/deftest test-p1
  (t/is (= 1656 (p1 example-data)))
  (t/is (= 1603 (p1 puzzle-input))))

(defn p2 [energies]
  (->> (iterate (fn [{:keys [energies]}]
                  (step energies))
                {:energies energies, :flashes 0})
       (take-while #(not= (:flashes %) 100))
       count))

(t/deftest test-p2
  (t/is (= 195 (p2 example-data)))
  (t/is (= 222 (p2 puzzle-input))))
