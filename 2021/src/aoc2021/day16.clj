(ns aoc2021.day16
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(def puzzle-input
  (str/trim (slurp (io/resource "day16.txt"))))

(defn hex->binary [hex]
  (->> (for [ch hex]
         (-> (Byte/parseByte (str ch) 16)
             Integer/toBinaryString
             Integer/parseInt
             (->> (format "%04d"))))
       (str/join)))

(defn read-packet [binary]
  (letfn [(read-literal [binary]
            (loop [binary binary
                   groups []]
              (let [group (subs binary 0 5)
                    binary (subs binary 5)
                    continue? (pos? (Byte/parseByte (subs group 0 1) 2))
                    groups (conj groups (subs group 1))]
                (if continue?
                  (recur binary groups)
                  [{:literal (Long/parseLong (str/join groups) 2)}
                   binary]))))

          (read-op0 [binary]
            (let [sub-packet-len (Integer/parseInt (subs binary 0 15) 2)]
              [(loop [binary (subs binary 15 (+ 15 sub-packet-len))
                      packets []]
                 (if (seq binary)
                   (let [[packet binary] (read-packet binary)]
                     (recur binary (conj packets packet)))
                   {:packets packets}))
               (subs binary (+ 15 sub-packet-len))]))

          (read-op1 [binary]
            (let [sub-packet-count (Integer/parseInt (subs binary 0 11) 2)
                  [packets binary] (-> (iterate (fn [[packets binary]]
                                                  (let [[packet binary] (read-packet binary)]
                                                    [(conj packets packet) binary]))
                                                [[] (subs binary 11)])
                                       (nth sub-packet-count))]

              [{:packets packets} binary]))]

    (let [version (Byte/parseByte (subs binary 0 3) 2)
          binary (subs binary 3)
          type-id (Byte/parseByte (subs binary 0 3) 2)
          binary (subs binary 3)]
      (-> (if (= 4 type-id)
            (read-literal binary)

            (let [length-type-id (Byte/parseByte (subs binary 0 1) 2)
                  binary (subs binary 1)]
              (case length-type-id
                0 (read-op0 binary)
                1 (read-op1 binary))))
          (update 0 assoc :version version, :type-id type-id)))))

(defn parse-packet [hex]
  (first (read-packet (hex->binary hex))))

(t/deftest test-parse-packet
  (t/is (= {:literal 2021, :version 6, :type-id 4}
           (parse-packet "D2FE28")))

  (t/is (= {:packets [{:literal 1, :version 2, :type-id 4}
                      {:literal 2, :version 4, :type-id 4}
                      {:literal 3, :version 1, :type-id 4}],
            :version 7, :type-id 3}
           (parse-packet "EE00D40C823060")))

  (t/is (= {:packets [{:packets [{:version 5,
                                  :packets [{:literal 15, :version 6, :type-id 4}]
                                  :type-id 2}],
                       :version 1, :type-id 2}],
            :version 4, :type-id 2}
           (parse-packet "8A004A801A8002F478"))))

(defn sum-versions [{:keys [version packets]}]
  (apply + version (map sum-versions packets)))

(def p1 (comp sum-versions parse-packet))

(t/deftest test-p1
  (t/is (= 16 (p1 "8A004A801A8002F478")))
  (t/is (= 12 (p1 "620080001611562C8802118E34")))
  (t/is (= 23 (p1 "C0015000016115A2E0802F182340")))
  (t/is (= 31 (p1 "A0016C880162017C3686B18A3D4780")))

  (t/is (= 904 (p1 puzzle-input))))

(defn eval-packet [{:keys [type-id packets] :as packet}]
  (let [sub-values (mapv eval-packet packets)]
    (case type-id
      0 (apply + sub-values)
      1 (apply * sub-values)
      2 (apply min sub-values)
      3 (apply max sub-values)
      4 (:literal packet)
      5 (if (apply > sub-values) 1 0)
      6 (if (apply < sub-values) 1 0)
      7 (if (apply = sub-values) 1 0))))

(def p2 (comp eval-packet parse-packet))

(t/deftest test-p2
  (t/is (= 3 (p2 "C200B40A82")))
  (t/is (= 54 (p2 "04005AC33890")))
  (t/is (= 7 (p2 "880086C3E88112")))
  (t/is (= 9 (p2 "CE00C43D881120")))
  (t/is (= 1 (p2 "D8005AC2A8F0")))
  (t/is (= 0 (p2 "F600BC2D8F")))
  (t/is (= 0 (p2 "9C005AC2F8F0")))
  (t/is (= 1 (p2 "9C0141080250320F1802104A08")))

  (t/is (= 200476472872 (p2 puzzle-input))))
