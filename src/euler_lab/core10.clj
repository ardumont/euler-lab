(ns euler-lab.core10
  "Problem 10 - http://projecteuler.net/problem=10
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million."
  (:require [midje.sweet :as m]))

                                        ; brute force

;(defn sum-prime-number-bf "Compute the sum of all prime number below the borne - Brute force"
;  [borne]
;(reduce +
;(filter #(<= % borne)
;        (euler-lab.core7/prime-numbers-improved borne)
;)))

;.;. Before the reward there must be labor. You plant before you
;.;. harvest. You sow in tears before you reap joy. -- Ransom
;(fact
;  (sum-prime-number-bf 0) => 0
;  (sum-prime-number-bf 1) => 0
;  (sum-prime-number-bf 2) => 2
;  (sum-prime-number-bf 3) => 5
;  (sum-prime-number-bf 4) => 5
;  (sum-prime-number-bf 5) => 10
;  (sum-prime-number-bf 7) => 17
;  (sum-prime-number-bf 2000000) => 142913828922
;)

                                        ; More subtle

(defn sum-prime-numbers "Return the list of the n first prime numbers"
  [borne]
  (if (or (zero? borne) (== 1 borne))
    0
    (loop [candidate 3 sum 2 primes [2]]
    (if (< borne candidate)
      sum
      (if (not (some #(zero? (rem candidate %)) (take (Math/floor (Math/sqrt candidate)) primes)))
        (recur (+ 2 candidate) (+ sum candidate) (conj primes candidate))
        (recur (+ 2 candidate) sum primes))))))

(m/fact
  (sum-prime-numbers 0) => 0
  (sum-prime-numbers 1) => 0
  (sum-prime-numbers 2) => 2
  (sum-prime-numbers 3) => 5
  (sum-prime-numbers 4) => 5
  (sum-prime-numbers 5) => 10
  (sum-prime-numbers 7) => 17
  (sum-prime-numbers 2000000) => 142913828922)
