(ns advent-of-code.day17)

(def input 337)

(defn p1 [input]
  (->> (reduce (fn [{:keys [buffer pos]} n]
                 (let [next-pos (mod (+ pos input) (count buffer))]
                   {:pos (inc next-pos)
                    :buffer (into (subvec buffer 0 (inc next-pos))
                                  (cons n (subvec buffer (inc next-pos))))}))
               {:buffer [0], :pos 0}
               (range 1 (inc 2017)))
       :buffer
       (drop-while (complement #{2017}))
       second))

;;; I'm sure there's probably a more mathematical way to do this, but this runs
;;; in a few seconds and I've got to walk the dog ;)
(defn p2 [input]
  (:res (reduce (fn [{:keys [pos res]} n]
                  (let [next-pos (mod (+ pos input) n)]
                    {:pos (inc next-pos)
                     :res (if (zero? next-pos) n res)}))
                {:pos 0}
                (range 1 (inc 50e6)))))
