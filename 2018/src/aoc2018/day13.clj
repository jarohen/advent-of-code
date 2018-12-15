(ns aoc2018.day13
  (:require [aoc2018.util :as util]
            [clojure.string :as str]
            [sss.arrows :refer [->% ->>%]]
            [clojure.test :as t]))

(def east [1 0])
(def west [-1 0])
(def north [0 -1])
(def south [0 1])

(def left {east north, north west, west south, south east})
(def straight identity)
(def right (into {} (map (juxt val key)) left))

(defn parse-input [lines]
  (let [cells (->> (map-indexed (fn [y line]
                                  (keep-indexed (fn [x cell]
                                                  (when-not (= \space cell)
                                                    (merge {:x x, :y y}
                                                           (case cell
                                                             \- {:track identity}
                                                             \> {:track identity, :cart-dir east}
                                                             \< {:track identity, :cart-dir west}

                                                             \| {:track identity}
                                                             \^ {:track identity, :cart-dir north}
                                                             \v {:track identity, :cart-dir south}

                                                             \/ {:track {east north, south west, west south, north east}}
                                                             \\ {:track {west north, north west, east south, south east}}

                                                             \+ {:track :intersection}))))
                                                line))
                                lines)
                   (apply concat))]

    {:tracks (into {}
                   (map (fn [{:keys [x y track]}]
                          [[x y] track]))
                   cells)

     :carts (into []
                  (keep (fn [{:keys [x y cart-dir]}]
                          (when cart-dir
                            {:loc [x y] :dir cart-dir,
                             :turns (cycle [left straight right])})))
                  cells)}))

(defn move-straight [{:keys [loc dir] :as cart}]
  (merge cart
         (let [[x y] loc
               [dx dy] dir]
           {:loc [(+ x dx) (+ y dy)]})))

(defn move-cart [{:keys [loc dir turns] :as cart} {:keys [tracks]}]
  (let [track (get tracks loc)]
    (when-not track
      (throw (ex-info "oh no" {:cart (dissoc cart :turns)
                               :tracks  tracks})))
    (move-straight (merge cart
                          (if (= :intersection track)
                            (let [[turn & turns] turns]
                              {:dir (turn dir), :turns turns})

                            {:dir (track dir)})))))

(defn step [{:keys [tracks carts]}]
  (let [{:keys [crashes carts]} (->> (sort-by (juxt :y :x) carts)

                                     (reduce (fn [{:keys [carts crashes] :as acc} {:keys [loc] :as cart}]
                                               (if (contains? crashes loc)
                                                 acc

                                                 (let [carts (dissoc carts loc)
                                                       {:keys [loc] :as new-cart} (move-cart cart {:tracks tracks})]
                                                   (if (contains? carts loc)
                                                     {:carts (dissoc carts loc)
                                                      :crashes (conj crashes loc)}
                                                     {:carts (assoc carts loc new-cart)
                                                      :crashes crashes}))))

                                             {:carts (into {} (map (juxt :loc identity)) carts)
                                              :crashes #{}}))]

    {:tracks tracks
     :carts (vals carts)
     :crashes crashes}))

(defn first-crash [input]
  (->> input
       (iterate step)
       (mapcat :crashes)
       first))

(defn last-cart [input]
  (->> input
       (iterate step)
       (drop-while (->% :carts count (> 1)))
       first :carts first :loc))

(t/deftest test-first-crash
  (t/is (= [7 3]
           (first-crash (parse-input ["/->-\\        "
                                      "|   |  /----\\"
                                      "| /-+--+-\\  |"
                                      "| | |  | v  |"
                                      "\\-+-/  \\-+--/"
                                      "  \\------/     "]))))

  (t/is (= [50 54] (util/with-line-seq "day13.txt" (->% parse-input first-crash)))))

(t/deftest test-last-cart
  (t/is (= [6 4]
           (last-cart (parse-input ["/>-<\\  "
                                    "|   |  "
                                    "| /<+-\\"
                                    "| | | v"
                                    "\\>+</ |"
                                    "  |   ^"
                                    "  \\<->/"]))))

  ;; broken
  (t/is (= [50 100] (util/with-line-seq "day13.txt" (->% parse-input last-cart)))))
