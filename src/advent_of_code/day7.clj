(ns advent-of-code.day7
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-input [s]
  (-> s
      (s/split-lines)
      (->> (map (comp #(re-matches #"(\w+) \((\d+)\)( -> (.+))?" %) s/trim))
           (into {} (map (fn [[_ id weight-str _ above-str]]
                           [id {:weight (Long/parseLong weight-str)
                                :above (when above-str
                                         (into #{} (s/split above-str #", ")))}]))))))

(def input
  (->> (io/resource "day7.txt")
       slurp s/trim
       parse-input))

(defn pt1 [input]
  (set/difference (set (keys input))
                  (into #{} (mapcat :above (vals input)))))

(defn pt2 [input]
  (let [root (first (pt1 input))]
    (loop [[[op id] & more] [[:expand root]]
           combined-weights {}]
      (when op
        (let [{:keys [weight above]} (get input id)]
          (case op
            :expand (recur (concat (for [el above]
                                     [:expand el])
                                   [[:calc id]]
                                   more)
                           combined-weights)

            :calc (let [above-els (into #{} (map (fn [el] [el (get combined-weights el)])) above)
                        above-weights (into [] (map second above-els))]
                    (if (and (seq above-weights) (apply not= above-weights))
                      (for [[id combined-weight] above-els]
                        [id (get-in input [id :weight]) combined-weight])

                      (recur more
                             (assoc combined-weights id (apply + weight above-weights)))))))))))
