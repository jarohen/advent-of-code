(ns aoc2018.day4
  (:require [aoc2018.files :as files]
            [sss.arrows :refer [->%]]))

(defn parse-line [line]
  (-> (zipmap [:year :month :day :hour :minute :event :guard-id]
              (rest (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (Guard #(\d+) begins shift|falls asleep|wakes up)" line)))
      (update :year files/parse-long)
      (update :month files/parse-long)
      (update :day files/parse-long)
      (update :hour files/parse-long)
      (update :minute files/parse-long)
      (update :guard-id (->% (some-> files/parse-long)))
      ))


(defn calc-asleep [evs]
  (-> (reduce (fn [{:keys [current-guard-id min-asleep guards-asleep] :as acc} {:keys [minute event guard-id]}]
                (cond
                  guard-id (assoc acc :current-guard-id guard-id)
                  (= event "falls asleep") (assoc acc :min-asleep minute)
                  (= event "wakes up") {:current-guard-id (:current-guard-id acc)
                                        :min-asleep nil
                                        :guards-asleep (update guards-asleep current-guard-id
                                                               (fn [mins-asleep]
                                                                 (reduce (fn [acc min]
                                                                           (update acc min (fnil inc 0)))
                                                                         mins-asleep
                                                                         (range min-asleep minute))))}))

              {:guards-asleep {}}
              evs)

      :guards-asleep))

(files/with-line-seq "day4.txt"
  (fn [lines]
    (->> (calc-asleep (map parse-line lines))
         (into [] (mapcat (fn [[guard-id sleepies]]
                            (for [[min freq] sleepies]
                              {:guard-id guard-id
                               :min min
                               :freq freq})
                            )))
         (sort-by :freq >))))

(files/with-line-seq "day4.txt"
  (fn [lines]
    (-> (calc-asleep (map parse-line lines))
        (get 3491)
        (->> (sort-by val >))
        first)))

()


(def sample
  ["[1518-11-01 00:00] Guard #10 begins shift"
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
   "[1518-11-05 00:55] wakes up"])
