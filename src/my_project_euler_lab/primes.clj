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
          ))))
  )

;.;. Woohoo! -- @zspencer
(fact
  (prime-numbers-improved 5) => [2 3 5 7 11]
  (prime-numbers-improved 10) => [2 3 5 7 11 13 17 19 23 29]
  (prime-numbers-improved 15) => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47]
  )

; this one, i do not like, the decomposition of divisor is terrible!
(defn dec-prime-number "Compute the decomposition in prime numbers."
  [n]
  (cond
   (<= n 1) []
   (== 2 n) [2]
   :else
   (let [primes (prime-numbers-improved (ceil (sqrt n)))
         divs (filter #(not= nil %) (map #(if (zero? (rem %1 %2)) %2) (repeat (count primes) n) primes))]
     (loop [acc-all-divisors [] acc-div-primes divs num n]
       (if (= 1 num)
         acc-all-divisors
         (if (= nil (first acc-div-primes))
           [n]
           (let [divisor-prime (first acc-div-primes)]
             (if (zero? (rem num divisor-prime))
               (recur (conj acc-all-divisors divisor-prime) acc-div-primes (/ num divisor-prime))
               (recur acc-all-divisors (rest acc-div-primes) num)))))))))

(fact
  (dec-prime-number 0)            => []
  (dec-prime-number 1)            => []
  (dec-prime-number 2)            => [2]
  (dec-prime-number 3)            => [3]
  (dec-prime-number 4)            => [2 2]
  (dec-prime-number 5)            => [5]
  (dec-prime-number 10)           => [2 5]
  (dec-prime-number 28)           => [2 2 7]
  (dec-prime-number 53)           => [53]
  (dec-prime-number 67)           => [67]
  (dec-prime-number 76)           => [2 2 19]
  (dec-prime-number 100)          => [2 2 5 5]
  )
