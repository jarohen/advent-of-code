(ns advent-of-code.day15)

(def input
  {:a {:initial-val 883, :factor 16807}
   :b {:initial-val 879, :factor 48271}})

(def example-input
  {:a {:initial-val 65, :factor 16807}
   :b {:initial-val 8921, :factor 48271}})

(def modulus 2147483647)

(defn generate-seq [{:keys [initial-val factor]}]
  (rest (iterate #(rem (* % factor) modulus) initial-val)))

(defn bits-for-comparison [x]
  (bit-and (dec (bit-shift-left 1 16)) x))

(defn p1 [input]
  (->> (map vector
            (map bits-for-comparison (generate-seq (:a input)))
            (map bits-for-comparison (generate-seq (:b input))))
       (take 40e6)
       (filter #(apply = %))
       (count)))

(defn p2 [input]
  (->> (map vector
            (map bits-for-comparison (filter #(zero? (mod % 4)) (generate-seq (:a input))))
            (map bits-for-comparison (filter #(zero? (mod % 8)) (generate-seq (:b input)))))
       (take 5e6)
       (filter #(apply = %))
       (count)))
