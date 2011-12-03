(ns my-project-euler-lab.core-pb-7
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

                                        ; Problem 7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
;; we can see that the 6th prime is 13.
;; What is the 10 001st prime number?

(defn prime-numbers "Return the list of the n first prime numbers"
  [n]
  (loop [candidate 2 current n primes []]
    (if (zero? current)
      primes
      (if (every? #(not= 0 (rem candidate %)) primes)
        (recur (inc candidate) (dec current) (cons candidate primes))
        (recur (inc candidate) current primes)
        )))
  )
                                        ; list of the n primes number

(fact
  (prime-numbers 0) => '()
  (prime-numbers 1) => '(2)
  (prime-numbers 2) => '(3 2)
  (prime-numbers 3) => '(5 3 2)
  (prime-numbers 4) => '(7 5 3 2)
  (prime-numbers 100) => '(541 523 521 509 503 499 491 487 479 467 463 461 457 449 443 439 433 431 421 419 409 401 397 389 383 379 373 367 359 353 349 347 337 331 317 313 311 307 293 283 281 277 271 269 263 257 251 241 239 233 229 227 223 211 199 197 193 191 181 179 173 167 163 157 151 149 139 137 131 127 113 109 107 103 101 97 89 83 79 73 71 67 61 59 53 47 43 41 37 31 29 23 19 17 13 11 7 5 3 2)
  )

;(fact (first (prime-numbers 10001)) => 104743)

; all primes except 2 are odd, so starting from 3, we can increment
; with 2.

; Another principles tells us that to see if a number is a prime, it
; suffices to check that the remainder is 0 for the prime from 2 to
; sqrt of the number to test

; return expression every? not= 0 in not some zero?
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
          ))))
  )

;.;. Simplicity, carried to the extreme, becomes elegance. -- Jon Franklin
(fact
  (prime-numbers-improved 0) => '()
  (prime-numbers-improved 1) => '(2)
  (prime-numbers-improved 2) => '(2 3)
  (prime-numbers-improved 3) => '(2 3 5)
  (prime-numbers-improved 4) => '(2 3 5 7)
  (prime-numbers-improved 100) => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541)
  )

;(fact (first (prime-numbers-improved 10001)) => 104743)

