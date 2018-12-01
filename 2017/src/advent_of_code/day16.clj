(ns advent-of-code.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (-> (slurp (io/resource "day16.txt"))
      s/trim
      (s/split #",")))

(defn parse-move [move-str]
  (case (first move-str)
    \s {:move-type :spin, :cnt (Long/parseLong (subs move-str 1))}
    \x {:move-type :exchange
        :swap (into #{} (map #(Long/parseLong %)) (s/split (subs move-str 1) #"/"))}
    \p {:move-type :partner
        :swap (into #{} (s/split (subs move-str 1) #"/"))}))

(def start
  (mapv str "abcdefghijklmnop"))

(defn apply-move [dancers {:keys [move-type cnt swap]}]
  (case move-type
    :spin (let [dancer-count (count start)
                pivot (- dancer-count cnt)]
            (into (subvec dancers pivot dancer-count) (subvec dancers 0 pivot)))

    :exchange (let [[x1 x2] (seq swap)]
                (-> dancers
                    (assoc x1 (nth dancers x2))
                    (assoc x2 (nth dancers x1))))

    :partner (let [[d1 d2] (seq swap)]
               (->> dancers
                    (into [] (map (some-fn {d1 d2, d2 d1} identity)))))))

(defn apply-round [dancers moves]
  (reduce apply-move dancers moves))

(defn p1 [input]
  (s/join (apply-round start (map parse-move input))))

(defn p2 [input]
  (let [moves (map parse-move input)]
    (loop [cur start
           history {}]
      (if-let [idx (get history cur)]
        (let [inverted-history (into {} (map (fn [[k v]] [v k])) history)]
          (s/join (get inverted-history (long (rem 1e6 (count history))))))
        (recur (apply-round cur moves) (assoc history cur (count history)))))))
