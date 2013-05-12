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
  "Compute the division the elementary school way :D"
  [num den]
  (->> {:c? true :n num :q 0}
        (iterate
         (fn [{:keys [n]}]
           (let [r (rem n den)
                 qu (quot n den)]
             (cond (or (= 0 r) (= r den)) {:c? false :n 0        :q qu}
                   (< r den)              {:c? true  :n (* 10 r) :q qu}
                   :else                  {:c? true  :n r        :q qu}))))
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
       (map :q)) => [0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1])

(comment
)
