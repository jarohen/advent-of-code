(ns advent-of-code.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [sss.arrows :refer :all]))

(def input
  (->> (io/resource "day8.txt") slurp s/split-lines
       (map (->% (s/split #"\s")
                 (->> (zipmap [:reg :op :arg :if :cmp-reg :cmp :cmp-arg]))
                 (update :op {"inc" +, "dec" -})
                 (update :cmp {"<" <, "<=" <=, "!=" not=, "==" =, ">" >, ">=" >=})
                 (update :arg #(Long/parseLong %))
                 (update :cmp-arg #(Long/parseLong %))
                 (dissoc :if)))))

(defn pt1 [input]
  (let [regs (reduce (fn [regs {:keys [op reg arg, cmp cmp-reg cmp-arg] :as el}]
                       (cond-> regs
                         (cmp (get regs cmp-reg 0) cmp-arg) (update reg (fnil op 0) arg)))
                     {}
                     input)]
    (apply max (vals regs))))

(defn pt2 [input]
  (-> (reduce (fn [{:keys [regs max-val] :as env} {:keys [op reg arg, cmp cmp-reg cmp-arg] :as el}]
                (if (cmp (get regs cmp-reg 0) cmp-arg)
                  (let [new-val (op (get regs reg 0) arg)]
                    {:regs (assoc regs reg new-val)
                     :max-val (max max-val new-val)})

                  env))

              {:max-val 0, :regs {}}
              input)
      :max-val))
