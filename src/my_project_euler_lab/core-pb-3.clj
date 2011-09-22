(ns my-project-euler-lab.core
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; problem3

;Th prime factors of 13195 are 5, 7, 13 and 29.
;What is the largest prime factor of the number 600851475143 ?

(defn prime-numbers "Return the list of the n first prime numbers"
  [n]
  (loop [candidate 2 current n primes []]
    (if (zero? current)
      primes
      (if (every? #(not= 0 (rem candidate %)) primes)
        (recur (inc candidate) (dec current) (conj primes candidate))
        (recur (inc candidate) current primes)
        )))
  
  )

(fact (prime-numbers 0) => '())
(fact (prime-numbers 1) => '(2))
(fact (prime-numbers 2) => '(2 3))
(fact (prime-numbers 3) => '(2 3 5))
;.;. Excellence is not an act but a habit. -- Aristotle
(fact (prime-numbers 4) => '(2 3 5 7))


