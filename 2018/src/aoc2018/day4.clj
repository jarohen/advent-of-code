(ns aoc2018.day4
  (:require [aoc2018.files :as files]
            [clojure.test :as t]
            [sss.arrows :refer [->%]]))

(defn parse-line [line]
  (-> (zipmap [:year :month :day :hour :minute :event :guard-id]
              (rest (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (Guard #(\d+) begins shift|falls asleep|wakes up)" line)))
      (update :minute files/parse-long)
      (update :guard-id (->% (some-> files/parse-long)))))

(defn ->naps [evs]
  (-> (reduce (fn [{:keys [current-guard-id fell-asleep naps] :as acc} {:keys [minute event guard-id]}]
                (cond
                  guard-id (assoc acc :current-guard-id guard-id)
                  (= event "falls asleep") (assoc acc :fell-asleep minute)
                  (= event "wakes up") (-> acc
                                           (assoc :fell-asleep nil)
                                           (update-in [:naps current-guard-id]
                                                      (fn [naps]
                                                        (reduce (fn [acc min]
                                                                  (update acc min (fnil inc 0)))
                                                                naps
                                                                (range fell-asleep minute)))))))

              {}
              evs)

      :naps))

(defn ->most-sleepy-guard [naps]
  (let [{:keys [guard-id naps]} (->> naps
                                     (into [] (map (fn [[guard-id naps]]
                                                     {:guard-id guard-id
                                                      :mins-asleep (transduce (map val) + naps)
                                                      :naps naps})))

                                     (sort-by :mins-asleep >)
                                     first)
        sleepy-min (->> naps (sort-by val >) first key)]

    {:guard-id guard-id
     :sleepy-min sleepy-min
     :res (* guard-id sleepy-min)}))

(defn ->most-sleepy-guard-min [naps]
  (let [{:keys [guard-id sleepy-min]} (->> (for [[guard-id naps] naps
                                                 [sleepy-min freq] naps]
                                             {:guard-id guard-id
                                              :sleepy-min sleepy-min
                                              :freq freq})

                                           (sort-by :freq >)

                                           first)]
    {:guard-id guard-id
     :sleepy-min sleepy-min
     :res (* guard-id sleepy-min)}))

(defn e2e [lines]
  (let [naps (->> lines
                  (map parse-line)
                  ->naps)]
    {:most-sleepy-guard (->most-sleepy-guard naps)
     :most-sleepy-guard-min (->most-sleepy-guard-min naps)}))

(t/deftest test-sleepy-guards
  (let [sample ["[1518-11-01 00:00] Guard #10 begins shift"
                "[1518-11-01 00:05] falls asleep"
                "[1518-11-01 00:25] wakes up"
                "[1518-11-01 00:30] falls asleep"
                "[1518-11-01 00:55] wakes up"
                "[1518-11-01 23:58] Guard #99 begins shift"
                "[1518-11-02 00:40] falls asleep"
                "[1518-11-02 00:50] wakes up"
                "[1518-11-03 00:05] Guard #10 begins shift"
                "[1518-11-03 00:24] falls asleep"
                "[1518-11-03 00:29] wakes up"
                "[1518-11-04 00:02] Guard #99 begins shift"
                "[1518-11-04 00:36] falls asleep"
                "[1518-11-04 00:46] wakes up"
                "[1518-11-05 00:03] Guard #99 begins shift"
                "[1518-11-05 00:45] falls asleep"
                "[1518-11-05 00:55] wakes up"]]

    (t/is (= {:most-sleepy-guard {:guard-id 10, :sleepy-min 24, :res 240}
              :most-sleepy-guard-min {:guard-id 99, :sleepy-min 45, :res 4455}}
             (e2e sample)))))

(comment
  (files/with-line-seq "day4.txt" e2e))
