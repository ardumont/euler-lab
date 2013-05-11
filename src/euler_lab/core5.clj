(ns euler-lab.core5
  "Problem 5 - http://projecteuler.net/problem=5
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
  (:require [midje.sweet :as m]))

;; brute force

(defn euler-5-ok? "Can the number be divided by each of the numbers from 1 to borne without any remainder?"
  [num borne]
  (not (some #(not= 0 %) (map #(rem num %) (range 2 (inc borne))))))

(m/fact
  (euler-5-ok? 2520 10) => true
  (euler-5-ok? 2521 10) => false)

(defn find-smallest-ok-euler-5 "Find the smallest number that check the predicate euler-5-ok?"
  [incr start rg]
  (first (take 1 (drop-while #(not (euler-5-ok? % rg)) (iterate #(+ incr %) start)))))

(m/fact (find-smallest-ok-euler-5 2 10 10) => 2520)
;(m/fact (find-smallest-ok-euler-5 2 20 20) => 232792560)
