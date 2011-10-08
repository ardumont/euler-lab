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

(defn prime? "Is the number a prime?"
  [n primes-seq]
  (not (some #(zero? (rem n %)) (take (floor (sqrt n)) primes-seq)))
  )

(fact
  (prime? 11 [2 3 5]) => true
  (prime? 12 [2 3 5]) => false
  (prime? 67 [2 3 5 7 11 13 17]) => true
  )

(defn lazy-primes "A lazy seq of primes, using the 'n is prime if not divisible by an int > sqrt n' optimization"
  ([]  (lazy-primes 2 []))
  ([n prevs]
     (lazy-seq
      (cons n
            (lazy-primes
             (loop [curr (inc n)]
               (if (prime? curr prevs)
                 curr
                 (recur (inc curr))))
             (conj prevs n))))))

;.;. O frabjous day! Callooh! Callay! -- Lewis Carroll
(fact
  (take 5 (lazy-primes))   => [2 3 5 7 11]
  (take 10 (lazy-primes))  => [2 3 5 7 11 13 17 19 23 29]
  (take 15 (lazy-primes))  => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47]
  (take 100 (lazy-primes)) => [ 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541]
)
