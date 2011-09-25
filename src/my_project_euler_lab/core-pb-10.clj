(ns my-project-euler-lab.core-pb-10
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))
                                        ; problem 10

;The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;Find the sum of all the primes below two million.

                                        ; brute force

;(reduce + (filter #(<= 2000000 %) (prime-numbers-improved 2000000)))

                                        ; More subtle

(defn sum-prime-numbers "Return the list of the n first prime numbers"
  [borne]
  (if (zero? borne)
    0
    (loop [candidate 3 sum 2 primes [2]]
    (if (<= borne candidate)
      sum
      (if (every? #(not= 0 (rem candidate %)) (take (floor (sqrt candidate)) primes))
        (recur (+ 2 candidate) (+ sum candidate) (conj primes candidate))
        (recur (+ 2 candidate) sum primes)
        )))
    )
  )

(fact
  (sum-prime-numbers 0) => 0
)

