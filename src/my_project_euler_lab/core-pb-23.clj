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

(unfinished abundant2? )

(defn all-dec-in-sums "Find all the sums possibles that gives n with 2 integers"
  [n]
  (let [half-n (ceil (/ n 2))
        integers-s (range 1 (inc half-n))]
    (reduce conj #{}
            (map
             #(vec [% (- n %)])
             integers-s))))

(fact
  (all-dec-in-sums 10) => #{[1 9] [2 8] [3 7] [4 6] [5 5]}
  (all-dec-in-sums 11) => #{[6 5] [5 6] [4 7] [3 8] [2 9] [1 10]}
  )

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
  (abundant? 120) => true
  )

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
    (abundant? 9) => false)
  )

(fact
  (sum-two-abundant? 10) => true
  (provided
    (all-dec-in-sums 10) => #{[5 9] [8 2]}
    (abundant? 5) => true
    (abundant? 9) => true)
  )

; it test
(fact
  (sum-two-abundant? 60) => true
  (sum-two-abundant? 120) => true
  (sum-two-abundant? 240) => true
  (sum-two-abundant? 28123) => true
  )

(defn map-abundants "Make a map of abundant numbers."
  [n]
  (reduce conj {} (map #(if (abundant? %) {% %}) (range 1 (inc n)))))

(fact
  (map-abundants 24) => {24 24, 20 20, 18 18, 12 12})

(defn abundant2? "Another implementation to check if a number is abundant or not."
  [m-abundants n]
  (not= nil (m-abundants n))
  )

(fact
  (abundant2? (map-abundants 24) 10) => false
  (abundant2? (map-abundants 24) 12) => true
  )

(defn sum-two-abundant2? ""
  [m-abundants n]
  (let [all-sums (all-dec-in-sums n)]
    (some #(and (abundant2? m-abundants (first %))
                (abundant2? m-abundants (second %))) all-sums)))

(fact
  (sum-two-abundant2? :map-abundant 10) => falsey
  (provided
    (all-dec-in-sums 10) => #{[5 5] [9 1]}
    (abundant2? :map-abundant 5) => nil
    (abundant2? :map-abundant 9) => nil)
  )

(fact
  (sum-two-abundant2? (map-abundants 100) 24) => true
  (sum-two-abundant2? (map-abundants 100) 10) => nil)

(defn sum-2-abundant-numbers "Compute the set of the numbers that can be written as the sum of two abundant numbers."
  [n]
  (set (map (fn [num] (if (sum-two-abundant? num) num))
            (range 1 (inc n)))))

(fact "Now they are real tests"
  (sum-2-abundant-numbers 50) => #{nil 32 36 38 40 42 44 48 50 24 30}
  (sum-2-abundant-numbers 100) => #{nil 32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94})

(defn sum-2-abundant-numbers2 "Compute the set of the numbers that can be written as the sum of two abundant numbers."
  [m-abundant n]
  (set (map (fn [num] (if (sum-two-abundant2? m-abundant num) num))
            (range 1 (inc n)))))

(fact "Now they are real tests"
  (sum-2-abundant-numbers2 (map-abundants 50)  50) => #{nil 32 36 38 40 42 44 48 50 24 30}
  (sum-2-abundant-numbers2 (map-abundants 100) 100) => #{nil 32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94})

(defn sum-all-positive-integer-euler-23-2 "Compute the sum of all positive integer which cannot be written as the sum of two abundants numbers in the interval 12 - n"
  [m-abundants n]
  (let [s-integers (conj (set (range 1 (inc n))) nil)
        s-sum2 (sum-2-abundant-numbers2 m-abundants n)]
    (reduce + (difference s-integers s-sum2))))

;(def map-abt (map-abundants 28123))

(fact "Test the reality with the second solution"
  (sum-all-positive-integer-euler-23-2 (map-abundants 100) 100) => 2766
  (sum-all-positive-integer-euler-23-2 (map-abundants 1000) 1000) => 240492
  )

(defn sum-all-positive-integer-euler-23 "Compute the sum of all positive integer which cannot be written as the sum of two abundants numbers in the interval 12 - n"
  [n]
  (sum-all-positive-integer-euler-23-2 (map-abundants n) n)
  )

;.;. The biggest reward for a thing well done is to have done it. --
;.;. Voltaire
(fact "Test the reality"
  (sum-all-positive-integer-euler-23 100) => 2766
  (sum-all-positive-integer-euler-23 1000) => 240492)

(println "--------- END OF PB 23 ----------" (java.util.Date.))
