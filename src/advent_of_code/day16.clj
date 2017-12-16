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

(defn move-fn [{:keys [move-type cnt swap]}]
  (case move-type
    :spin (let [dancer-count (count start)
                pivot (- dancer-count cnt)]
            (fn [dancers]
              (into (subvec dancers pivot dancer-count) (subvec dancers 0 pivot))))

    :exchange (let [[x1 x2] (seq swap)]
                (fn [dancers]
                  (-> dancers
                      (assoc x1 (nth dancers x2))
                      (assoc x2 (nth dancers x1)))))

    :partner (let [[d1 d2] (seq swap)
                   swap-fn (some-fn {d1 d2, d2 d1} identity)]
               (fn [dancers]
                 (->> dancers
                      (into [] (map swap-fn)))))))

(defn p1 [input]
  (->> (reduce #(%2 %1) start (map (comp move-fn parse-move) input))
       s/join))

(defn p2 [input]
  (->> (reduce #(%2 %1) start (apply concat (repeat 1e2 (map (comp move-fn parse-move) input))))
       s/join))
