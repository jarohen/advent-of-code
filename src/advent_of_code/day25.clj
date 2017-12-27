(ns advent-of-code.day25)

(def input 12368930)

(def rules
  {[:a 0] {:write 1, :state :b, :move 1}
   [:a 1] {:write 0, :move 1, :state :c}

   [:b 0] {:write 0, :state :a, :move -1}
   [:b 1] {:write 0, :state :d, :move 1}

   [:c 0] {:write 1, :move 1, :state :d}
   [:c 1] {:write 1, :move -1, :state :a}

   [:d 0] {:write 1, :move -1, :state :e}
   [:d 1] {:write 0, :move -1, :state :d}

   [:e 0] {:write 1, :move 1, :state :f}
   [:e 1] {:write 1, :move -1, :state :b}

   [:f 0] {:write 1, :move 1, :state :a}
   [:f 1] {:write 1, :move 1, :state :e}})

(defn p1 [{:keys [rules input]}]
  (loop [tape {}
         pos 0
         state :a
         steps 0]
    (cond
      (= steps input) (frequencies (vals tape))

      :else (let [{:keys [write state move]} (get rules [state (get tape pos 0)])]
              (recur (assoc tape pos write)
                     (+ pos move)
                     state
                     (inc steps))))))
