(ns aoc2020.day4
  (:require [aoc2020.util :as util]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn parse-pps [lines]
  (for [pp (->> lines
                (partition-by str/blank?)
                (remove #{[""]})
                (map #(str/join " " %)))]
    (->> (for [kv (str/split pp #" ")
               :let [[_ k v] (re-matches #"(.+):(.+)" kv)]]
           [k v])
         (into {}))))

(defn valid-p1? [pp]
  (every? pp ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn valid-p2? [{:strs [byr iyr eyr hgt hcl ecl pid] :as pp}]
  (and byr (re-matches #"\d{4}" byr) (<= 1920 (Long/parseLong byr) 2002)
       eyr (re-matches #"\d{4}" eyr) (<= 2020 (Long/parseLong eyr) 2030)
       iyr (re-matches #"\d{4}" iyr) (<= 2010 (Long/parseLong iyr) 2020)
       hgt (or (when-let [[_ cm] (re-matches #"(\d+)cm" hgt)]
                 (<= 150 (Long/parseLong cm) 193))
               (when-let [[_ in] (re-matches #"(\d+)in" hgt)]
                 (<= 59 (Long/parseLong in) 76)))
       hcl (re-matches #"#[0-9a-f]{6}" hcl)
       ecl (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       pid (re-matches #"\d{9}" pid)))

(comment
  (util/with-line-seq "day4.txt"
    (fn [lines]
      (let [pps (parse-pps lines)]
        [(count (filter valid-p1? pps))
         (count (filter valid-p2? pps))]))))

(t/deftest test-valid-p2
  (t/is (->> ["eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
              ""
              "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
              ""
              "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
              ""
              "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"]
             parse-pps
             (every? (complement valid-p2?))))
  (t/is (->> ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
              ""
              "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
              ""
              "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
              ""
              "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]
             parse-pps
             (every? valid-p2?))))
