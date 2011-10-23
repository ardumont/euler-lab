(ns my-project-euler-lab.core-pb-23
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.set :only [difference]] )
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.primes :only [all-divisors-bi]] )
  )

;A perfect number is a number for which the sum of its proper divisors
;is exactly equal to the number. 

;For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
;
;A number n is called deficient if the sum of its proper divisors is less than n 
;and it is called abundant if this sum exceeds n.

;As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;smallest number that can be written as the sum of two abundant
;numbers is 24. By mathematical analysis, it can be shown that all
;integers greater than 28123 can be written as the sum of two abundant
;numbers.

;However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number 
;that cannot be expressed as the sum of two abundant numbers is less than this limit.

;Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

(unfinished )

(defn all-dec-in-sums "Find all the sums possibles that gives n with 2 integers"
  [integers n]
  (let [half-n (ceil (/ n 2))
        integers-s (take half-n integers)]
    (reduce conj #{}
            (map
             #(vec [% (- n %)])
             integers-s))))

(fact
  (all-dec-in-sums (iterate inc 1) 10) => #{[1 9] [2 8] [3 7] [4 6] [5 5]}
  (all-dec-in-sums (iterate inc 1) 11) => #{[6 5] [5 6] [4 7] [3 8] [2 9] [1 10]}
  )

(defn abundant? "Test if a number is abundant (sums of its divisors is superior to itself)"
  [n]
  (let [sum-all-divisors (reduce + (all-divisors-bi n))]
    (< n sum-all-divisors)))

(fact
  (abundant? 10) => false
  (abundant? 120) => true
  )

(defn sum-two-abundant? "Can the number be a sum of 2 abundants numbers?"
  [set-integers-23 n]
  (let [all-sums (all-dec-in-sums set-integers-23 n)]
    (some #(and (abundant? (first %)) (abundant? (second %))) all-sums)))

(fact
  (sum-two-abundant? :set-integers-23 10) => falsey
  (provided
    (all-dec-in-sums :set-integers-23 10) => #{[5 5] [9 1]}
    (abundant? 5) => false
    (abundant? 9) => false)
  )

(fact
  (sum-two-abundant? :set-integers-23 10) => true
  (provided
    (all-dec-in-sums :set-integers-23 10) => #{[5 9] [8 2]}
    (abundant? 5) => true
    (abundant? 9) => true)
  )

; it test
(fact
  (sum-two-abundant? (iterate inc 0) 240) => true
  (sum-two-abundant? (iterate inc 0) 28123) => true
  )

(defn abundant-numbers
  [set-integers-23 n]
  (set (map (fn [num] (if (sum-two-abundant? set-integers-23 num) num))
             (take n set-integers-23))))

;.;. I do believe in praising that which deserves to be praised. -- Dean
;.;. Smith
(fact
  (abundant-numbers (iterate inc 1) 50) => #{nil 32 36 38 40 42 44 48 50 24 30}
  (abundant-numbers (iterate inc 1) 100) => #{nil 32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94})

(defn sum-all-positive-integer-euler-23 "Compute the sum of all positive integer which cannot be written as the sum of two abundant numbers in the interval 12 - n"
  [set-integers-23 n]
  (let [set-integers-23-s (conj (take n set-integers-23) nil)]
    (reduce +
            (difference
             set-integers-23-s
             (abundant-numbers set-integers-23 n)))))

(future-fact "Test the reality"
  (sum-all-positive-integer-euler-23 (iterate inc 1) 28123) => #{})

(println "--------- END OF PB 23 ----------" (java.util.Date.))
