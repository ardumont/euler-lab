(ns my-project-euler-lab.core-pb-0
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; Problem 1

; If we list all the natural numbers below 10 that are multiples of 3
; or 5, we get 3, 5, 6 and 9. 
; The sum of these multiples is 23.
; Find the sum of all the multiples of 3 or 5 below 1000.

(defn nn-mult-3-5 "Return the list of the natural numbers multiple of 3 or 5 below n"
  [n]
  (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range 1 n)) 
  )

(fact (nn-mult-3-5 10) => [3 5 6 9])
(fact (reduce + (nn-mult-3-5 10)) => 23)
(fact (reduce + (nn-mult-3-5 1000)) => 233168)

; more subtle
; compute the sum of element multiple by 3 and not by 5 or by 5 and
; not by 3 (at the same time)

; even more subtle
; sum 1 -> n = (* 1/2 n (inc n))

;Let’s look at the details of our function and take as example n=3.
;We would have to add:
;3+6+9+12+......+999=3*(1+2+3+4+...+333)
;For n=5 we would get:
;5+10+15+...+995=5*(1+2+....+199)
;Now note that 199=995/5 but also 999/5 rounded down to the nearest integer.
;In many programming languages there exists a separate operator for
;that: div or \.


