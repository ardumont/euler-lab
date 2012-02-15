(ns my-project-euler-lab.core-pb-6
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]]))

;; The sum of the squares of the first ten natural numbers is,
;; 12 + 22 + ... + 102 = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)2 = 552 = 3025

;; Hence the difference between the sum of the squares of the first
;; ten natural numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

(defn square "Compute the square of a number" [x] (* x x))

(fact
  (square 1) => 1
  (square 2) => 4
  (square 3) => 9
  (square 10) => 100
  (square 12) => 144
  (reduce + (map square (range 1 11))) => 385
  (square (reduce + (range 1 11))) => 3025)

(defn diff-sum-square-square-sum "Compute the difference between the sum of the square and the square of the sum"
  [range]
  (- (square (reduce + range)) (reduce + (map square range))))

(fact
  (diff-sum-square-square-sum (range 1 11)) => 2640
  (diff-sum-square-square-sum (range 1 101)) => 25164150)

