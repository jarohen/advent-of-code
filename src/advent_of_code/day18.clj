(ns advent-of-code.day18
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [instaparse.core :as i]))

(i/defparser parser
  "<Insn> = snd | rcv | add | mul | mod | set | jgz
   snd = <'snd '> val; rcv = <'rcv '> val
   add = <'add '> reg <space> val; mul = <'mul '> reg <space> val; mod = <'mod '> reg <space> val
   set = <'set '> reg <space> val
   jgz = <'jgz '> val <space> val

   <val> = reg | num
   reg = #'\\w+'
   num = #'-?\\d+'
   <space> = ' '")

(defn parse-insn [insn-str]
  (i/transform {:num (fn [num] [:num (Long/parseLong num)])} (parser insn-str)))

(def insns
  (into [] (mapcat parse-insn) (s/split-lines (slurp (io/resource "day18.txt")))))

(defn p1 [insns])

(defn eval-arg [[arg-type arg-val] regs]
  (case arg-type
    :reg (get regs arg-val 0)
    :num arg-val))

(defn p1 [insns]
  (loop [pc 0
         regs {}
         last-snd nil]
    (let [[insn & args] (nth insns pc)]
      (case insn
        :snd (recur (inc pc) regs (eval-arg (first args) regs))
        :rcv (if-not (zero? (eval-arg (first args) regs))
               last-snd
               (recur (inc pc) regs last-snd))
        :set (recur (inc pc)
                    (assoc regs (second (first args)) (eval-arg (second args) regs))
                    last-snd)

        :jgz (recur (+ pc (if (pos? (eval-arg (first args) regs))
                            (eval-arg (second args) regs)
                            1))
                    regs last-snd)

        (:add :mul :mod) (let [f (case insn :add +, :mul *, :mod mod)]
                           (recur (inc pc)
                                  (assoc regs (second (first args))
                                         (f (eval-arg (first args) regs)
                                            (eval-arg (second args) regs)))
                                  last-snd))))))

;; PART 2

(defn eval-insn [{:keys [pc regs q other-q] :as env} {:keys [insns]}]
  (let [[insn & args] (nth insns pc)]
    (case insn
      :snd (-> env
               (update :pc inc)
               (update :other-q (fnil conj []) (eval-arg (first args) regs)))

      :rcv (-> env
               (update :pc inc)
               (assoc-in [:regs (second (first args))] (first q))
               (update :q subvec 1))

      :set (-> env
               (update :pc inc)
               (assoc-in [:regs (second (first args))] (eval-arg (second args) regs)))

      :jgz (-> env
               (update :pc (fn [pc]
                             (+ pc (if (pos? (eval-arg (first args) regs))
                                     (eval-arg (second args) regs)
                                     1)))))

      (:add :mul :mod) (-> env
                           (update :pc inc)
                           (update-in [:regs (second (first args))]
                                      (fn [cur]
                                        (let [f (case insn :add +, :mul *, :mod mod)]
                                          (f cur
                                             (eval-arg (second args) regs)))))))))

(defn active? [{:keys [pc q]} insns]
  (and (<= 0 pc (dec (count insns)))
       (not (and (= (first (nth insns pc)) :rcv)
                 (empty? q)))))

(defn p2 [insns]
  (loop [envs [{:p 0, :pc 0, :regs {"p" 0}, :snd-count 0, :q [], :other-idx 1}
               {:p 1, :pc 0, :regs {"p" 1}, :snd-count 0, :q [], :other-idx 0}]]
    (if-let [active-programs (seq (filter #(active? % insns) envs))]
      (recur (reduce (fn [envs active-idx]
                       (let [{:keys [other-idx] :as env} (get envs active-idx)
                             other-q (get-in envs [other-idx :q])
                             other-q-count (count other-q)
                             new-env (eval-insn (assoc env :other-q other-q) {:insns insns})]

                         (-> envs
                             (assoc active-idx (-> new-env
                                                   (dissoc :other-q)
                                                   (cond-> (not= other-q-count (count (:other-q new-env))) (update :snd-count inc))))
                             (assoc-in [other-idx :q] (:other-q new-env)))))
                     envs
                     (map :p active-programs)))

      (get-in envs [1 :snd-count]))))
