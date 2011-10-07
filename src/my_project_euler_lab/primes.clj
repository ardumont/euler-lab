(ns my-project-euler-lab.primes
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

; memory, not for using
(defn prime-numbers-improved "Return the list of the n first prime numbers"
  [n]
  (if (zero? n)
    []
    (loop [candidate 3 current (dec n) primes [2]]
      (if (zero? current)
        primes
        (if (not (some #(zero? (rem candidate %)) (take (floor (sqrt candidate)) primes)))
          (recur (+ 2 candidate) (dec current) (conj primes candidate))
          (recur (+ 2 candidate) current primes)
          )))))

(fact
  (prime-numbers-improved 5) => [2 3 5 7 11]
  (prime-numbers-improved 10) => [2 3 5 7 11 13 17 19 23 29]
  (prime-numbers-improved 15) => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47]
  )

(defn dec-prime-numbers-with-primes "Compute the decomposition in prime numbers."
  [primes n]
  (cond
   (<= n 1) []
   :else
   (loop [acc-all-divisors [] acc-div-primes primes num n]
     (if (= 1 num)
       acc-all-divisors
       (let [divisor-prime (first acc-div-primes)]
         (if (zero? (rem num divisor-prime))
           (recur (conj acc-all-divisors divisor-prime) acc-div-primes (/ num divisor-prime))
           (recur acc-all-divisors (rest acc-div-primes) num)))))))

(fact
  (dec-prime-numbers-with-primes [] 0)               => []
  (dec-prime-numbers-with-primes [] 1)               => []
  (dec-prime-numbers-with-primes [2] 2)              => [2]
  (dec-prime-numbers-with-primes [2 3] 3)            => [3]
  (dec-prime-numbers-with-primes [2 3] 4)            => [2 2]
  (dec-prime-numbers-with-primes [2 3 5] 5)          => [5]
  (dec-prime-numbers-with-primes [2 3 5 7 11] 10)    => [2 5]
  (dec-prime-numbers-with-primes [2 3 5 7 11] 11)    => [11]
  (dec-prime-numbers-with-primes [2 3 5 7 11] 28)    => [2 2 7]
  (dec-prime-numbers-with-primes [2 3 5 7 11 53] 53) => [53]
  (dec-prime-numbers-with-primes [2 3 5 7 11 67] 67) => [67]
  (dec-prime-numbers-with-primes [2 3 5 7 11 19] 76) => [2 2 19]
  (dec-prime-numbers-with-primes [2 3 5 7 11] 100) => [2 2 5 5])

(defn dec-prime-numbers "Compute the decomposition in prime number of a number"
  [n]
  (let [primes (prime-numbers-improved n)]
    (dec-prime-numbers-with-primes primes n)))

;.;. Woohoo! -- @zspencer
(fact
  (dec-prime-numbers 0)            => []
  (dec-prime-numbers 1)            => []
  (dec-prime-numbers 2)            => [2]
  (dec-prime-numbers 3)            => [3]
  (dec-prime-numbers 4)            => [2 2]
  (dec-prime-numbers 5)            => [5]
  (dec-prime-numbers 10)           => [2 5]
  (dec-prime-numbers 11)           => [11]
  (dec-prime-numbers 28)           => [2 2 7]
  (dec-prime-numbers 53)           => [53]
  (dec-prime-numbers 67)           => [67]
  (dec-prime-numbers 76)           => [2 2 19]
  (dec-prime-numbers 100)          => [2 2 5 5]
  )
