(ns my-project-euler-lab.primes
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.set :only [difference]]))

; memory, not for using
(defn prime-numbers-improved "Return the list of the n first prime numbers"
  [n]
  (if (zero? n)
    []
    (loop [candidate 3 current (dec n) primes [2]]
      (if (zero? current)
        primes
        (if (not (some #(zero? (rem candidate %)) (take (Math/floor (Math/sqrt candidate)) primes)))
          (recur (+ 2 candidate) (dec current) (conj primes candidate))
          (recur (+ 2 candidate) current primes))))))

(fact
  (prime-numbers-improved 5) => [2 3 5 7 11]
  (prime-numbers-improved 10) => [2 3 5 7 11 13 17 19 23 29]
  (prime-numbers-improved 15) => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47])

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
  (dec-prime-numbers 100)          => [2 2 5 5])

(defn prime? "Is the number a prime?"
  [n primes-seq]
  (not-any? #(= 0 (rem n %)) (take (Math/floor (Math/sqrt n)) primes-seq)))

(fact
  (prime? 11 [2 3 5]) => true
  (prime? 12 [2 3 5]) => false
  (prime? 67 [2 3 5 7 11 13 17]) => true)

(defn lazy-primes "A lazy seq of primes, using the 'n is prime if not divisible by an int > sqrt n' optimization"
  ([]  (lazy-primes 2 []))
  ([n prevs]
     (lazy-seq
      (cons
       n
       (lazy-primes
        (loop [curr (inc n)]
          (if (prime? curr prevs)
            curr
            (recur (inc curr))))
        (conj prevs n))))))

(fact
  (take 5 (lazy-primes))   => [2 3 5 7 11]
  (take 10 (lazy-primes))  => [2 3 5 7 11 13 17 19 23 29]
  (take 15 (lazy-primes))  => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47]
  (take 100 (lazy-primes)) => [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541])

(defn all-divisors "Compute all divisors of a number (improved version)"
  [n]
  (cond
   (zero? n) "infinity"
   (= 1 n) [n]
   (= 2 n) [1 2]
   :else
   (loop [acc #{} num (int (Math/sqrt (inc n)))]
     (if (zero? num)
       acc
       (if (zero? (rem n num))
         (recur (conj acc num (/ n num)) (dec num)) 
         (recur acc (dec num)))))))

(fact
  (all-divisors 0) => "infinity"
  (all-divisors 1) => [1]
  (all-divisors 2) => (just [1 2] :in-any-order)
  (all-divisors 10) => (just [1 2 5 10] :in-any-order)
  (all-divisors 15) => (just [1 3 5 15] :in-any-order)
  (all-divisors 21) => (just [1 3 7 21] :in-any-order)
  (all-divisors 28) => (just [1 2 4 7 14 28] :in-any-order)
  (all-divisors 120) => (just [1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120] :in-any-order))

(defn all-divisors-bi "Compute all divisors of a number but itself."
  [n]
  (cond
   (zero? n) "infinity"
   (= 1 n) nil
   (= 2 n) [1]
   :else
   (loop [acc #{} num (int (Math/sqrt (inc n)))]
     (if (= 1 num)
       (conj acc 1)
       (if (zero? (rem n num))
         (recur (conj acc num (/ n num)) (dec num)) 
         (recur acc (dec num)))))))

(fact
  (all-divisors-bi 0) => "infinity"
  (all-divisors-bi 1) => nil
  (all-divisors-bi 2) => (contains [1] :in-any-order)
  (all-divisors-bi 10) => (contains [1 2 5] :in-any-order)
  (all-divisors-bi 15) => (contains [1 3 5] :in-any-order)
  (all-divisors-bi 21) => (contains [1 3 7] :in-any-order)
  (all-divisors-bi 28) => (contains [1 2 4 7 14] :in-any-order)
  (all-divisors-bi 120) => (contains [1 2 3 4 5 6 8 10 12 15 20 24 30 40 60] :in-any-order))

(defn count-divisors-with-dec-prime "Compute the number of divisors with the help of the decomposition in prime numbers."
  [primes n]
  (cond
   (<= n 1) 0;principles -> infinity
   :else
   (loop [nb-divisors 1, cnt-exp 0, acc-div-primes primes, num n]
     (if (= 1 num)
       (* nb-divisors (inc cnt-exp))
       (let [divisor-prime (first acc-div-primes)]
         (if (zero? (rem num divisor-prime))
           (recur nb-divisors (inc cnt-exp) acc-div-primes (/ num divisor-prime))
           (recur (* nb-divisors (inc cnt-exp)) 0 (rest acc-div-primes) num)))))))

(def little-primes [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107])

(fact
 (count-divisors-with-dec-prime [] 1) => 0
 (count-divisors-with-dec-prime [] 0) => 0
 (count-divisors-with-dec-prime little-primes 10) => 4
 (count-divisors-with-dec-prime little-primes 15) => 4
 (count-divisors-with-dec-prime little-primes 21) => 4
 (count-divisors-with-dec-prime little-primes 28) => 6
 (count-divisors-with-dec-prime (take 20 (lazy-primes)) 120) => 16)

(defn all-divisors-with-dec-prime "Compute the divisors with the help of the decomposition in prime numbers."
  [primes n]
  (cond
   (<= n 1) #{}
   :else
   (loop [acc-divisors #{}
          acc-div-primes primes
          num n]
     (if (= 1 num)
       acc-divisors
       (let [divisor-prime (first acc-div-primes)
             divisor-n (/ num divisor-prime)]
         (if (zero? (rem num divisor-prime))
           (recur (conj acc-divisors divisor-prime divisor-n) acc-div-primes divisor-n)
           (recur acc-divisors (rest acc-div-primes) num)))))))

(future-fact "Finish the implem all the divisors with the decomposition in prime number"
 (all-divisors-with-dec-prime [] 0) => #{}
 (all-divisors-with-dec-prime [] 1) => #{}
 (all-divisors-with-dec-prime little-primes 10) => #{1 2 5}
 (all-divisors-with-dec-prime little-primes 15) => #{1 3 5}
 (all-divisors-with-dec-prime little-primes 21) => #{1 3 7}
 (all-divisors-with-dec-prime little-primes 28) => #{1 2 4 7 14})

(comment
  (fact (all-divisors-with-dec-prime (take 20 (lazy-primes)) 120) => #{1 2 3 4 5 6 8 10 12 15 20 24 30 40 60}))

(println "--------- END OF PRIMES ----------" (java.util.Date.))
