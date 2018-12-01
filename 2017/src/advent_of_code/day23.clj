(ns advent-of-code.day23
  (:require [instaparse.core :as i :refer [defparser]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defparser parser
  "<Insns> = (<'\n'>? Insn)*
   <Insn> = set | sub | mul | jnz
   set = <'set '> reg <' '> val
   sub = <'sub '> reg <' '> val
   mul = <'mul '> reg <' '> val
   jnz = <'jnz '> val <' '> val

   <val> = reg | num
   reg = #'[a-z]'
   num = #'-?\\d+'")

(def insns
  (vec (i/transform {:num (fn [n] [:num (Long/parseLong n)])}
                    (parser (str/trim (slurp (io/resource "day23.txt")))))))

(defn step [[insn & args] {:keys [regs pc mul-count] :as env}]
  (let [eval (fn [[val-type val-arg]]
               (case val-type
                 :reg (get regs val-arg 0)
                 :num val-arg))]
    (merge env
           (case insn
             :set {:regs (assoc regs (second (first args)) (eval (second args)))
                   :pc (inc pc)}

             (:sub :mul) (let [f (case insn
                                   :sub -
                                   :mul *)]
                           {:regs (assoc regs (second (first args)) (f (eval (first args))
                                                                       (eval (second args))))
                            :pc (inc pc)
                            :mul-count (cond-> mul-count (= insn :mul) inc)})

             :jnz {:pc (+ pc
                          (if (zero? (eval (first args)))
                            1
                            (eval (second args))))}))))

(defn p1 [insns]
  (loop [{:keys [pc mul-count] :as env} {:pc 0
                                         :regs {}
                                         :mul-count 0}]
    (if-let [insn (get insns pc)]
      (recur (step (get insns pc) env))
      mul-count)))


(def primes-in-range
  (-> (str/trim (slurp (io/resource "day23-primes.txt")))
      (str/split #"\s+")
      (->> (into #{} (map #(Long/parseLong %))))))

(def p2
  (->> (range 106500 123500 17)
       (remove primes-in-range)
       count))
