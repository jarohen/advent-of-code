(ns advent-of-code.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (s/split-lines (slurp (io/resource "day13.txt")))
       (into {}
             (map (fn [line]
                    (let [[_ col depth] (re-matches #"(\d+): (\d+)" line)]
                      [(Long/parseLong col) (Long/parseLong depth)]))))))

(def fake-input {0 3, 1 2, 4 4, 6 4})

(defn caught-cols [{:keys [firewall]}]
  (into #{}
        (filter (fn [col]
                  (when-let [depth (get firewall col)]
                    (zero? (mod col (* 2 (dec depth)))))))
        (range (inc (apply max (keys firewall))))))

(defn p1 [firewall]
  (transduce (map #(* % (get firewall %)))
             +
             (caught-cols {:firewall firewall})))
