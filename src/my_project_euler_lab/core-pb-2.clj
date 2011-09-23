(ns my-project-euler-lab.core-pb-2
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; Problem 2

; Each new term in the Fibonacci sequence is generated by adding the
;previous two terms. By starting with 1 and 2, the first 10 terms will be:
;1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

;By considering the terms in the Fibonacci sequence whose values do
;not exceed four million, find the sum of the even-valued
                                        ;terms.

                                        ; fibonacci simple

(defn fibonacci "Fibonacci suite"
  [n]
  (if (= n 1)
    [1]
    (if (= n 2)
      [2 1]
      (loop [current 3 fib [2 1]]
      (if (> current n)
        fib
        (recur (inc current) (cons (+ (first fib) (second fib)) fib)))
      )))
  )

(fact (fibonacci 1) => [1])
(fact (fibonacci 2) => [2 1])
(fact (fibonacci 3) => [3 2 1])
(fact (fibonacci 4) => [5 3 2 1])
(fact (fibonacci 10) => [89 55 34 21 13 8 5 3 2 1])

                                        ; compute fibonacci

(defn fibonacci-borne "Fibonacci suite"
  [n borne]
  (if (= n 1)
    [1]
    (if (= n 2)
      [2 1]
      (loop [current 3 fib [2 1]]
        (let [ sum (+ (first fib) (second fib))]
          (if (or (> current n) (> sum borne))
            fib
            (recur (inc current) (cons sum fib)))
          )))
    ))

; compute the sum of even number in the fibonacci suite up to 4000000
(reduce + (filter #(zero? (rem % 2)) (fibonacci-borne 50 4000000)))

                                        ; denis implem

;compute the sum and the fibonacci simultaneously

                                        ; other pist
;(take 32 (iterate #(cons (+ (first %) (second %)) %) [2 1]))

      