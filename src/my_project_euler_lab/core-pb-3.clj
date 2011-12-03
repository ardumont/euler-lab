(ns my-project-euler-lab.core-pb-3
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]]))

                                        ; problem3

;;The prime factors of 13195 are 5, 7, 13 and 29.
;;What is the largest prime factor of the number 600851475143 ?

;; Implementations of computing primes from the lower to the top in
;; ascending order

(defn prime-numbers "Return the list of the n first prime numbers in ascending order"
  [n]
  (loop [candidate 2 current n primes []]
    (if (zero? current)
      primes
      (if (every? #(not= 0 (rem candidate %)) primes)
        (recur (inc candidate) (dec current) (conj primes candidate))
        (recur (inc candidate) current primes)))))

(fact
  (prime-numbers 0) => '()
  (prime-numbers 1) => '(2)
  (prime-numbers 2) => '(2 3)
  (prime-numbers 3) => '(2 3 5)
  (prime-numbers 4) => '(2 3 5 7))

; implementations of computing the primes from the lower to the top in
; descending order

(defn prime-numbers-desc "Return the list of the n first prime numbers in descending order"
  [n]
  (loop [candidate 2 current n primes []]
    (if (zero? current)
      primes
      (if (every? #(not= 0 (rem candidate %)) primes)
        (recur (inc candidate) (dec current) (cons candidate primes))
        (recur (inc candidate) current primes)))))

(fact
  (prime-numbers-desc 0) => '()
  (prime-numbers-desc 1) => '(2)
  (prime-numbers-desc 2) => '(3 2)
  (prime-numbers-desc 3) => '(5 3 2)
  (prime-numbers-desc 4) => '(7 5 3 2))

;(take-while #(< % 10) (prime-numbers 100))
; pdt -> product; cand -> candidate; curr -> current; max -> max
; primes; primes -> list of primes

(defn max-factor-prime "Compute the factor prime of a number"
  [n]
  (loop [pdt 1 cand 2 curr n max 2 primes []]
    (if (or (zero? curr) (= pdt n))
      max
      (if (every? #(not= 0 (rem cand %)) primes)
        (if (zero? (rem n cand))
          (recur (* pdt cand) (inc cand) (dec curr) cand (cons cand primes))
          (recur pdt (inc cand) (dec curr) max (cons cand primes)))
        (recur pdt (inc cand) curr max primes)))))

;.;. The journey is the reward. -- traditional
(fact
  (max-factor-prime 10) => 5
  (max-factor-prime 20) => 5
  (max-factor-prime 197) => 197
  (max-factor-prime 13195) => 29)
;  (max-factor-prime 600851475143) => 6857

