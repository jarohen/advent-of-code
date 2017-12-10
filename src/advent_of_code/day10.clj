(ns advent-of-code.day10
  (:require [clojure.string :as s]))

(def input
  "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110")

(defn rotate [knot {:keys [knot-size pos length]}]
  (let [after-length (- knot-size length)
        [before after] (->> (cycle knot)
                            (drop pos)
                            (split-at length))]
    (->> (concat (reverse before) (take after-length after))
         (cycle)
         (drop (- knot-size pos))
         (take knot-size))))

(defn apply-round [{:keys [pos skip knot]} {:keys [knot-size lengths]}]
  (reduce (fn [{:keys [pos skip knot]} length]
            {:pos (mod (+ pos length skip) knot-size)
             :skip (inc skip)
             :knot (rotate knot {:pos pos, :length length, :knot-size knot-size})})
          {:pos pos, :skip skip, :knot knot}
          lengths))

(defn pt1 [{:keys [knot-size input]}]
  (->> (apply-round {:pos 0, :skip 0, :knot (range knot-size)}
                    {:knot-size knot-size,
                     :lengths (->> (s/split input #",")
                                   (map #(Long/parseLong %)))})
       :knot
       (take 2)
       (apply *)))

(def standard-suffix-lengths
  [17 31 73 47 23])

(defn sparse-hash [bytes {:keys [knot-size rounds]}]
  (let [lengths (concat bytes standard-suffix-lengths)]
    (-> (iterate #(apply-round % {:knot-size knot-size
                                  :lengths lengths})
                 {:pos 0, :skip 0, :knot (range knot-size)})
        (nth rounds)
        :knot)))

(defn sparse->dense [sparse]
  (->> (partition 16 sparse)
       (map #(apply bit-xor %))))

(defn ->hex [bytes]
  (->> bytes (map #(format "%02x" %)) s/join))

(defn pt2 [input]
  (-> (map byte input)
      (sparse-hash {:knot-size 256, :rounds 64})
      sparse->dense
      ->hex))
