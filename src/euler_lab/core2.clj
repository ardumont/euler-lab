(ns euler-lab.core2
  "Problem 2 - http://projecteuler.net/problem=2
Each new term in the Fibonacci sequence is generated by adding the
previous two terms. By starting with 1 and 2, the first 10 terms will be:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do
not exceed four million, find the sum of the even-valued terms."
  (:require [midje.sweet :as m]))

(defn fibonacci-seq "Lazy-seq fibonacci"
  []
  ((fn fib-seq [a b]
     (lazy-seq (cons a (fib-seq b (+ a b))))) 1 2))

(m/fact
  (take 1 (fibonacci-seq)) => [1]
  (take 2 (fibonacci-seq)) => [1 2]
  (take 3 (fibonacci-seq)) => [1 2 3]
  (take 4 (fibonacci-seq)) => [1 2 3 5]
  (take 11 (fibonacci-seq)) => [1 2 3 5 8 13 21 34 55 89 144]
  (reduce + (filter #(zero? (rem % 2)) (take 33 (fibonacci-seq)))) => 4613732)
