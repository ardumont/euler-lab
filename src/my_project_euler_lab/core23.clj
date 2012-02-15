(ns my-project-euler-lab.core23
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.set :only [difference union]] )
  (:use [my-project-euler-lab.primes :only [all-divisors-bi]]))

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

(unfinished)

(defn all-dec-in-sums "Find all the sums possibles that gives n with 2 integers"
  [n]
  (let [half-n (Math/ceil (/ n 2))
        integers-s (range 1 (inc half-n))]
    (reduce conj #{}
            (map
             #(vec [% (- n %)])
             integers-s))))

(fact;;; proceed to jack in

  (all-dec-in-sums 10) => #{[1 9] [2 8] [3 7] [4 6] [5 5]}
  (all-dec-in-sums 11) => #{[6 5] [5 6] [4 7] [3 8] [2 9] [1 10]})

(defn abundant? "Test if a number is abundant (sums of its divisors is strictly superior to itself)"
  [n]
  (let [sum-all-divisors (reduce + (all-divisors-bi n))]
    (< n sum-all-divisors)))

(fact
  (abundant? 10) => false
  (abundant? 20) => true
  (abundant? 40) => true
  (abundant? 12) => true
  (abundant? 24) => true
  (abundant? 48) => true
  (abundant? 96) => true
  (abundant? 60) => true
  (abundant? 120) => true)

; visibly, if one number is abundant, its double is too. Furthermore,
; the double is a number that can be represented as a sum of two
; abundants numbers

(defn sum-two-abundant? "Can the number be a sum of 2 abundants numbers?"
  [n]
  (let [all-sums (all-dec-in-sums n)]
    (some #(and (abundant? (first %)) (abundant? (second %))) all-sums)))

(fact
  (sum-two-abundant? 10) => falsey
  (provided
    (all-dec-in-sums 10) => #{[5 5] [9 1]}
    (abundant? 5) => false
    (abundant? 9) => false))

(fact
  (sum-two-abundant? 10) => true
  (provided
    (all-dec-in-sums 10) => #{[5 9] [8 2]}
    (abundant? 5) => true
    (abundant? 9) => true))

; it test
(fact
  (sum-two-abundant? 60) => true
  (sum-two-abundant? 120) => true
  (sum-two-abundant? 240) => true
  (sum-two-abundant? 28123) => true)

(defn sum-2-abundant-numbers "Compute the set of the numbers that can be written as the sum of two abundant numbers."
  [n]
  (filter #(sum-two-abundant? %)
              (range 1 (inc n))))

(defn sum-all-positive-integer-euler-23 "Compute the sum of all positive integer which cannot be written as the sum of two abundants numbers in the interval 12 - n"
  [n]
  (let [s-integers (range 1 (inc n))
        s-sum2 (sum-2-abundant-numbers n)]
    (reduce + (difference (set s-integers) (set s-sum2)))))

(fact "Test the reality with the second solution"
  (sum-all-positive-integer-euler-23 100) => 2766
  (sum-all-positive-integer-euler-23 1000) => 240492)

; check a theory of mine - if a number is abundant, every double is!
;(let [m-abundant (map-abundants 1000)] (every? #(abundant2? map-abundants %) (init-sum-abundants m-abundant 1000))) => true

; another check ; every double of abundant number check the condition asked by the problem
;(let [m-abundant (map-abundants 1000)] (every? #(sum-two-abundant2? ;map-abundants %) (init-sum-abundants m-abundant 1000))) => true

(println "--------- END OF PB 23-0 ----------" (java.util.Date.))

