(ns my-project-euler-lab.core-pb-1-1
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]]))

                                        ; Problem 1

;; If we list all the natural numbers below 10 that are multiples of 3
;; or 5, we get 3, 5, 6 and 9. 
;; The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

(unfinished )

(defn borne-max "Compute the last number max to be multiple of n"
  [n]
  (loop [curr 999]
    (if (zero? (rem curr n))
      curr
      (recur (dec curr)))))

(fact "Compute the last number max to be multiple of n"
  (borne-max 3) => 999
  (borne-max 5) => 995
  (borne-max 15) => 990)

(defn sum-1-borne "Compute the sum from 1 to n"
  [n]
;  (reduce + (range 1 (inc n)))
  (* n 1/2 (inc n)))

(fact
  (sum-1-borne 10) => 55)

(defn sum-multiple-of-n "Compute the sum of all the multiples of n in the interval 1-999"
  [n]
  (let [borne (borne-max n)
        sum (sum-1-borne (/ borne n))]
    (* n sum)))

(fact "Mock"
  (sum-multiple-of-n 3) => 166833
  (provided
    (borne-max 3) => 999
    (sum-1-borne 333) => 55611))

;.;. The biggest reward for a thing well done is to have done it. --
;.;. Voltaire
(fact "Mock2"
    (sum-multiple-of-n 5) => 99500
  (provided
    (borne-max 5) => 995
    (sum-1-borne 199) => 19900))

(fact "Real test on the sum of multiples"
  (sum-multiple-of-n 3) => 166833
  (sum-multiple-of-n 5) => 99500
  (sum-multiple-of-n 15) => 33165)

; (+ 1 2 ... 333) -> set of multiple of 3 ; (* 3 333) => 999
; (+ 1 2 ... 199) -> set of multiple of 5 ; (* 5 199) => 995
; but in these sets, we have the multiple of 15 that are multiples of
; 3 and 5 so we must avoid them so the solution to the problems

(defn sum-multiple-of-3-and-5 "Compute the sum of all the multiples of 3 and 5 in the interval 1-999"
  []
  (- (+ (sum-multiple-of-n 3) (sum-multiple-of-n 5)) (sum-multiple-of-n 15)))

(fact "Mock test to find the sum of multiple of 3 and 5 in the interval 1 - 999"
  (sum-multiple-of-3-and-5) => 100
  (provided
    (sum-multiple-of-n 3) => 10
    (sum-multiple-of-n 5) => 100
    (sum-multiple-of-n 15) => 10))

(fact "real test"
  (sum-multiple-of-3-and-5) => 233168)

(println "--------- END OF PB 1-1 ----------" (java.util.Date.))
