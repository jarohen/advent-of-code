(ns aoc2018.files
  (:require [clojure.java.io :as io]))

(defn with-line-seq [resource f]
  (with-open [rdr (io/reader (io/resource resource))]
    (f (line-seq rdr))))

(defn parse-long [s] (Long/parseLong s))
