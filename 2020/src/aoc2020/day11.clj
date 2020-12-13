(ns aoc2020.day11
  (:require [aoc2020.util :as util]
            [clojure.test :as t]
            [sss.arrows :refer [->% ->>%]]))

(defn parse-seats [rows]
  (vec (for [row rows]
         (vec (for [col row]
                (case col
                  \. :no-seat
                  \L :empty
                  \# :occupied))))))

(def test-case
  (parse-seats ["L.LL.LL.LL"
                "LLLLLLL.LL"
                "L.L.L..L.."
                "LLLL.LL.LL"
                "L.LL.LL.LL"
                "L.LLLLL.LL"
                "..L.L....."
                "LLLLLLLLLL"
                "L.LLLLLL.L"
                "L.LLLLL.LL"]))

(defn tick [seats {:keys [over-occupied adjacent-seats]}]
  (vec (for [row (range (count seats))]
         (vec (for [col (range (count (get seats row)))]
                (let [occupied (-> (frequencies (adjacent-seats seats [row col]))
                                   (get :occupied 0))]
                  (case (get-in seats [row col])
                    :empty (if (zero? occupied) :occupied :empty)
                    :occupied (if (>= occupied over-occupied) :empty :occupied)
                    :no-seat :no-seat)))))))

(defn d11 [seats rules]
  (->> seats
       (util/fixpoint (->% (tick rules)))
       (mapcat seq)
       frequencies
       :occupied))

(def p1
  (->% (d11 {:over-occupied 4,
             :adjacent-seats (fn [seats [row col]]
                               (for [dir util/directions]
                                 (get-in seats (mapv + [row col] dir))))})))

(def p2
  (->% (d11 {:over-occupied 5,
             :adjacent-seats (fn [seats [row col]]
                               (for [dir util/directions]
                                 (->> dir
                                      (iterate (->>% (mapv + dir)))
                                      (map (fn [dir]
                                             (get-in seats (mapv + [row col] dir))))
                                      (take-while some?) ; stop when we've gone off the map
                                      (drop-while #{:no-seat})
                                      first)))})))

(t/deftest test-d11
  (t/is (= 37 (p1 test-case)))
  (t/is (= 26 (p2 test-case))))

(comment
  (util/with-line-seq "day11.txt"
    (->% parse-seats ((juxt p1 p2)))))
