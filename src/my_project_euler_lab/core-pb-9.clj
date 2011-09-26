(ns my-project-euler-lab.core-pb-9
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a^2 + b^2 = c^2

;For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.

; (1) a < b < c
; (2) a^2 + b^2 = c^2
; (3) a+b+c=1000 <=> a+b=1000-c 

; (3) <=> (a + b)^2 = (1000 - c)^2
; (3) <=> a^2 + b^2 + 2ab = 1000^2 - 2000c + c^2
; (3) <=> a^2 + b^2 = 1000^2 - 2000c + c^2 - 2ab
; (3),(2) <=> c^2 = 1000^2 - 2000c + c^2 - 2ab
; (3),(2) <=> 2000c - 1000^2 = -2ab
; (3),(2) <=> 500 (1000 - c) = ab
; (3),(2) <=> 1000 = ab/500 - c

(defn pythagorean-triplet? "Compute a^2 + b^2 = c^2"
  [a b c]
  (== (+ (* a a) (* b b)) (* c c)))

(fact
  (pythagorean-triplet? 1 2 3) => false
  (pythagorean-triplet? 3 4 5) => true
 )

(defn sum-1000? "Is the sum of 3 elements 1000?"
  [a b c]
  (= 1000 (+ a b c)))

;.;. Happiness is not a reward / it is a consequence. -- Ingersoll
(fact
  (sum-1000? 1000 0 0) => true
  (sum-1000? 500 500 1) => false
  )

(defn is-triplet-ok? "Is the triplet is ok according to the hypothesis of the problem?"
  [a b c]
  (and (< a b c) (= 1000 (+ a b c)) (== (+ (* a a) (* b b)) (* c c))))

(fact
  is-triplet-ok? [1 2 3] => false
  is-triplet-ok? [3 4 5] => false
  )
