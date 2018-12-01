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

(defn caught-cols [{:keys [delay firewall]}]
  (into #{}
        (filter (fn [col]
                  (when-let [depth (get firewall col)]
                    (zero? (mod (+ col delay) (* 2 (dec depth)))))))
        (keys firewall)))

(defn p1 [firewall]
  (transduce (map #(* % (get firewall %)))
             +
             (caught-cols {:delay 0, :firewall firewall})))

(defn p2 [firewall]
  (first (filter (comp empty? #(caught-cols {:delay %, :firewall firewall})) (range))))
