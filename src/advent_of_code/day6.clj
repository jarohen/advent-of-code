(ns advent-of-code.day6
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def input
   (-> (io/resource "day6.txt")
       slurp s/trim (s/split #"\s")
       (->> (map #(Long/parseLong %)))))

(defn max-idx [input]
  (reduce (fn [[max-idx max-val] [idx el]]
            (if (< max-val el)
              [idx el]
              [max-idx max-val]))
          (map vector (range) input)))

(defn step [input]
  (let [[max-idx max-val] (max-idx input)
        len (count input)
        quotient (quot max-val len)
        remainder (rem max-val len)]
    (->> (for [[idx val] (map vector (range) input)]
           (+ (if (= idx max-idx)
                0
                (cond-> val
                  (<= (mod (- idx max-idx) len) remainder) inc))
              quotient))
         (into []))))

(defn pt1+2 [input]
  (loop [seen {input 0}
         cur input]
    (let [next (step cur)
          idx (count seen)]
      (if-let [seen-idx (get seen next)]
        [(count seen) (- idx seen-idx)]
        (recur (assoc seen next idx) next)))))
