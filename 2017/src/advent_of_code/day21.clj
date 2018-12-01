(ns advent-of-code.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def start-sq
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

(defn parse-square [s]
  (into [] (map vec) (s/split s #"/")))

(defn parse-rule [rule]
  (let [[pre post] (s/split rule #" => ")]
    [(parse-square pre) (parse-square post)]))

(def rules
  (into {} (map parse-rule) (s/split-lines (slurp (io/resource "day21.txt")))))

(defn rotate [sq]
  (let [size (count sq)]
    (vec (for [x (range size)]
           (vec (for [y (range size)]
                  (get-in sq [(- size y 1) x])))))))

(defn flip [sq]
  (mapv (comp vec reverse) sq))

(defn perms [sq]
  (into #{} (concat (take 4 (iterate rotate sq))
                    (take 4 (iterate rotate (flip sq))))))

(def nxt
  (-> (fn [sq rules]
        (some rules (perms sq)))
      memoize))

(defn split-sq [sq]
  (letfn [(split-sq* [x]
            (vec (for [row (partition x sq)]
                   (apply mapv vector (mapv #(map vec (partition x %)) row)))))]
    (cond
      (zero? (mod (count sq) 2)) (split-sq* 2)
      (zero? (mod (count sq) 3)) (split-sq* 3))))

(defn combine-sqs [sqs]
  (vec (for [row sqs
             x (range (count (first row)))]
         (into [] (mapcat #(nth % x)) row))))

(defn expand [sq rules]
  (combine-sqs (for [row (split-sq sq)]
                 (for [sq row]
                   (nxt sq rules)))))

(defn play [{:keys [rules n]}]
  (-> (iterate #(expand % rules) start-sq)
      (nth n)
      (->> (apply concat))
      frequencies
      (get \#)))

(defn p1 [rules]
  (play {:rules rules, :n 5}))

(defn p2 [rules]
  (play {:rules rules, :n 18}))
