(ns euler-lab.core1
  "Problem 1 - http://projecteuler.net/problem=1

If we list all the natural numbers below 10 that are multiples of 3
or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000."
  (:require [midje.sweet :as m]))

(defn nn-mult-3-5 "Return the list of the natural numbers multiple of 3 or 5 below n"
  [n]
  (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range 1 n)) )

(m/fact
  (nn-mult-3-5 10)              => [3 5 6 9]
  (reduce + (nn-mult-3-5 10))   => 23
  (reduce + (nn-mult-3-5 1000)) => 233168)
