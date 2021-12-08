(ns aoc2021.day8
  (:require [clojure.string :as str]
            [aoc2021.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]
            [clojure.set :as set]))

(def example-data (str/trim "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(defn parse-line [line]
  (let [[input output] (str/split line #" \| ")]
    {:input (mapv set (str/split input #" "))
     :output (mapv set (str/split output #" "))}))

(defn p1 [data]
  (->> (mapcat :output data)
       (filter (comp #{2 3 4 7} count))
       count))

(t/deftest test-p1
  (t/is (= 26 (p1 (map parse-line (str/split-lines example-data)))))
  (t/is (= 375 (util/with-line-seq "day8.txt" (->>% (map parse-line) p1)))))

(def digits
  {(set "abcefg") 0
   (set "cf") 1
   (set "acdeg") 2
   (set "acdfg") 3
   (set "bcdf") 4
   (set "abdfg") 5
   (set "abdefg") 6
   (set "acf") 7
   (set "abcdefg") 8
   (set "abcdfg") 9})

(defn ->signal-mapping [input-digits]
  (let [freqs (->> input-digits (apply concat) frequencies)]
    (letfn [(find-digit-with-count [digit-count]
              (->> input-digits (into #{} (filter (->% count (= digit-count))))))

            (find-signal-with-freq [freq]
              (->> freqs (into #{}
                               (comp (filter (->% val (= freq)))
                                     (map key)))))]

      (let [e (first (find-signal-with-freq 4))
            f (first (find-signal-with-freq 9))
            b (first (find-signal-with-freq 6))

            dg (find-signal-with-freq 7)
            four (first (find-digit-with-count 4))
            d (first (set/intersection dg four))
            g (first (disj dg d))

            ac (find-signal-with-freq 8)
            one (first (find-digit-with-count 2))
            c (first (set/intersection one ac))
            a (first (disj ac c))]

        {a \a, b \b, c \c, d \d, e \e, f \f, g \g}))))

(defn ->digit-mapping [signal-mapping digit]
  (get digits (into #{} (map signal-mapping) digit)))

(defn solve-p2-line [{:keys [input output]}]
  (let [signal-mapping (->signal-mapping input)
        digit-mapping (->> input
                           (into {} (map (juxt identity (partial ->digit-mapping signal-mapping)))))]
    (->> (map digit-mapping output) str/join util/parse-long)))

(defn p2 [data]
  (transduce (map solve-p2-line) + data))

(t/deftest test-p2
  (t/is (= 61229 (p2 (map parse-line (str/split-lines example-data)))))
  (t/is (= 1019355 (util/with-line-seq "day8.txt" (->>% (map parse-line) p2)))))
