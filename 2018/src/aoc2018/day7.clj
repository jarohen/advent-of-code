(ns aoc2018.day7
  (:require [aoc2018.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as t]
            [traversy.lens :as tl]
            [sss.arrows :refer [->% ->>%]]))

(defn parse-line [line]
  (->> line
       (re-matches #"Step (.) must be finished before step (.) can begin\.")
       rest
       (map first)))

(defn ->nodes [lines]
  (let [g (->> lines
               (map parse-line)
               (reduce (fn [g [pre post]]
                         (update g post (fnil conj #{}) pre))
                       {}))]
    (merge (->> g
                (into #{} (mapcat val))
                (into {} (map (juxt identity (constantly #{})))))
           g)))

(defn assign-work [{:keys [nodes current-work]}]
  (reduce (fn [{:keys [current-work available] :as acc} current-node]
            (if (not= current-node :idle)
              (update acc :current-work conj current-node)

              (let [[available-node & more-available] available]
                {:current-work (conj current-work (or available-node :idle))
                 :available more-available})))

          {:current-work []
           :available (->> (filter (comp empty? :deps val) nodes)
                           (remove (comp (set current-work) key))
                           (map key)
                           sort)}
          current-work))

(defn step [{:keys [nodes current-work]}]
  (let [{:keys [current-work]} (assign-work {:nodes nodes, :current-work current-work})

        nodes (into nodes
                    (map (fn [{:keys [node cost deps]}]
                           [node {:node node
                                  :cost (dec cost),
                                  :deps deps}]))
                    (keep nodes current-work))

        done (into #{}
                   (comp (keep nodes)
                         (filter (comp zero? :cost))
                         (map :node))
                   current-work)]

    {:nodes (-> (apply dissoc nodes done)
                (tl/update (tl/*> tl/all-values (tl/in [:deps]))
                           (->% (set/difference done))))
     :current-work (map (some-fn (zipmap done (repeat :idle)) identity) current-work)
     :done (sort done)}))

(defn schedule [nodes {:keys [worker-count base-cost] :or {base-cost 0}}]
  (let [steps (->> {:nodes (into {}
                                 (map (fn [[node deps]]
                                        [node {:node node
                                               :cost (+ base-cost
                                                        (inc (- (int node) (int \A))))
                                               :deps deps}]))
                                 nodes)
                    :current-work (repeat worker-count :idle)}
                   (iterate step)
                   (take-while (some-fn (comp not-empty :nodes)
                                        (comp not-empty :done)))
                   rest)]

    {:steps steps
     :order (->> steps (mapcat :done) str/join)
     :ticks (count steps)}))

(t/deftest test-scheduler
  (let [sample ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."]]

    (t/is (= "CABDFE" (:order (schedule (->nodes sample) {:worker-count 1}))))

    (t/is (= 15 (:ticks (schedule (->nodes sample) {:worker-count 2})))))

  (t/is (= "ABLCFNSXZPRHVEGUYKDIMQTWJO"
           (util/with-line-seq "day7.txt"
             (->% ->nodes
                  (schedule {:worker-count 1})
                  :order)))))

(comment
  (util/with-line-seq "day7.txt"
    (->% ->nodes
         (schedule {:worker-count 1})
         :order))

  (util/with-line-seq "day7.txt"
    (->% ->nodes
         (schedule {:worker-count 5
                    :base-cost 60})
         :ticks)))
