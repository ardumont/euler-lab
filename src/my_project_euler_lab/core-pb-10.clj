(ns my-project-euler-lab.core-pb-10
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math)
  ;(:use my-project-euler-lab.core-pb-7)
  )
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
    (if (< borne candidate)
      sum
      (if (every? #(not= 0 (rem candidate %)) (take (floor (sqrt candidate)) primes))
        (recur (+ 2 candidate) (+ sum candidate) (conj primes candidate))
        (recur (+ 2 candidate) sum primes)
        )))
    )
  )

;.;. Work joyfully and peacefully, knowing that right thoughts and right
;.;. efforts will inevitably bring about right results. -- Allen
(fact
  (sum-prime-numbers 0) => 0
  (sum-prime-numbers 1) => 2
  (sum-prime-numbers 2) => 2
  (sum-prime-numbers 3) => 5
  (sum-prime-numbers 4) => 5
  (sum-prime-numbers 5) => 10
  (sum-prime-numbers 7) => 17
)

