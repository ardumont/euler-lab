(ns euler-lab.core26
  "Problem 26 - http://projecteuler.net/problem=26
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
    1/2  = 	0.5
    1/3  = 	0.(3)
    1/4  = 	0.25
    1/5  = 	0.2
    1/6  = 	0.1(6)
    1/7  = 	0.(142857)
    1/8  = 	0.125
    1/9  = 	0.(1)
    1/10 = 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part."
  (:require [clojure.string :as string]
            [midje.sweet    :as m]))

(defn tw
  "'take-while pred coll' with the first element that wrong the predicate pred."
  [pred coll]
  (let [[h t] (split-with pred coll)]
    (concat h [(first t)])))

(m/fact
  (tw even? [0 2 4 6 8 9 10 11 12]) => [0 2 4 6 8 9]
  (tw even? [1 2 3 4])              => [1])

(defn division
  "Compute the division the elementary school way :D (D is denominator, N numerator, R remains, Q quotient)."
  [N D]
  (->> {:c? true :n N}
       (iterate
        (fn [{:keys [n]}]
          (let [R (rem n D)
                Q (quot n D)]
            (cond (or (= 0 R) (= R D)) {:c? false :n 0        :q Q}
                  (< R D)              {:c? true  :n (* 10 R) :q Q}
                  :else                {:c? true  :n R        :q Q}))))
       (drop 1)))

(m/fact :now-we-have-a-way-to-divide-infinitely
  (->> (division 1 8)
       (tw :c?)
       (map :q)) => [0 1 2 5]
  (->> (division 1 20)
       (tw :c?)
       (map :q)) => [0 0 5]
  (->> (division 1 7)
       (take 20)
       (map :q)) => [0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1]
  (->> (division 1 7)
       (take 20)
       (map :n)) => [10 30 20 60 40 50 10 30 20 60 40 50 10 30 20 60 40 50 10 30]
  (->> (division 1 13)
       (take 20)
       (map :q)) => [0 0 7 6 9 2 3 0 7 6 9 2 3 0 7 6 9 2 3 0]
  (->> (division 1 13)
       (take 20)
       (map :n)) => [10 100 90 120 30 40 10 100 90 120 30 40 10 100 90 120 30 40 10 100])

(defn recurring-cycle-count
  "Compute the size of a recurring cycle in a sequence."
  [s]
  (->> {:v s :m {} :i 0}
       (iterate
        (fn [{:keys [v m i] :as cp}]
          (let [[h & t] v
                p       (m h)]
            (if p
              (assoc cp :r (- i p))
              {:v t :m (assoc m h i) :i (+ 1 i)}))))
       (drop-while #(nil? (:r %)))
       first
       :r))

(m/fact
  (recurring-cycle-count [10 30 20 60 40 50 10 30 20 60 40 50 10 30 20 60 40 50 10 30]) => 6
  (recurring-cycle-count [0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1])                     => 6
  (recurring-cycle-count [0 1 2 3 4 1 2 3 4])                                           => 4
  (recurring-cycle-count [0 1 2 5 2 5 2 5])                                             => 2)

(def recurring-cycle ^{:doc "Compute the recurring cycle from a division by 1"}
  (comp recurring-cycle-count
        (partial map :n)
        (partial division 1)))

(m/tabular
 (m/fact
   (recurring-cycle ?n) => ?r)
    ?n ?r
    2  1
    3  1
    4  1
    5  1
    6  1
    7  6
    8  1
    9  1
    10 1
    13 6)

(defn max-recurring-cycle
  "Given a limit l, return the couple [longest-recurring-cycle number] from 1 to (l-1), which corresponds to the number for which 1/number has the longest recurring cycle."
  [l]
  (->> (range 1 l)
       (map (juxt recurring-cycle identity))
       (into (sorted-map))
       last))

(m/fact
  (max-recurring-cycle 1000) => [982 983])

;; euler-lab.core26> (time (max-recurring-cycle 1000))
;; "Elapsed time: 237.337448 msecs"
;; [982 983]
