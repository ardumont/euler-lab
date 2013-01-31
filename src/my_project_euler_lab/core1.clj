(ns my-project-euler-lab.core1
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]]))

                                        ; Problem 1

; If we list all the natural numbers below 10 that are multiples of 3
; or 5, we get 3, 5, 6 and 9.
; The sum of these multiples is 23.
; Find the sum of all the multiples of 3 or 5 below 1000.

(defn nn-mult-3-5 "Return the list of the natural numbers multiple of 3 or 5 below n"
  [n]
  (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range 1 n)) )

(fact (nn-mult-3-5 10) => [3 5 6 9])
(fact (reduce + (nn-mult-3-5 10)) => 23)
;.;. Out of clutter find simplicity; from discord find harmony; in the
;.;.                               ; middle of difficulty lies
;.;.                               ; opportunity. -- Einstein
(fact (reduce + (nn-mult-3-5 1000)) => 233168)

(println "--------- END OF PB 1 ----------" (java.util.Date.))
