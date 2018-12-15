(ns aoc2018.day11)

(defn power-level [x y serial]
  (let [rack-id (+ (inc x) 10)]
    (-> (* rack-id (inc y))
        (+ serial)
        (* rack-id)
        (quot 100)
        (mod 10)
        (- 5))))

(defn ->power-levels [serial]
  (->> (for [x (range 300)
             y (range 300)]
         [[x y] (power-level x y serial)])
       (into {})))

(defn outer-cells [x y size]
  (set (concat (for [y (range y (+ y size))]
                 [(+ x (dec size)) y])
               (for [x (range x (+ y size))]
                 [x (+ y (dec size))]))))

(comment
  (time
   (let [serial 2694
         power-levels (->power-levels serial)
         size 300]
     (->> (for [x (range size)
                y (range size)]
            [x y])
          (pmap (fn [[x y]]
                  (doall (rest (reductions (fn [{:keys [power]} size]
                                             {:x x,
                                              :y y,
                                              :size size
                                              :power (+ power
                                                        (->> (for [[x y] (outer-cells x y size)]
                                                               (get power-levels [x y]))
                                                             (reduce +)))})
                                           {:power 0}
                                           (range 1 3))))))
          (apply concat)

          (sort-by :power >)
          first))))
