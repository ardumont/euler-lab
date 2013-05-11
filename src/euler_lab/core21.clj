(ns euler-lab.core21
  (:use [midje.sweet]
         [euler-lab.primes :only [all-divisors]]))

;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
;; 22, 44, 55 and 110; therefore d(220) = 284.
;; The proper divisors of 284 are 1, 2, 4, 71 and 142
;; so d(284) = 220.

;; Evaluate the sum of all the amicable numbers under 10000.

(defn all-divisors-bl "Retrieve all the divisors except the last one (which is the numbers that starts all)"
  [num]
  (if (= 1 num)
    []
    (butlast (all-divisors num))))

(fact
  (all-divisors-bl 10) => [1 2 5]
  (all-divisors-bl 1) => []
  (all-divisors-bl 2) => [1])

(defn amicable? "Heart of the problem - determine if a number is amicable or not"
  [num]
  (if (= 1 num)
    false
    (let [nb-divisors (all-divisors-bl num)
          sum-divisors (reduce + nb-divisors)
          nb-divisors-sum (reduce + (all-divisors-bl sum-divisors))]
      #_    (println "num" num "nb-divisors" nb-divisors "sum-divisors" sum-divisors "nb-divisors" nb-divisors)
      (and (not= num sum-divisors) (= num nb-divisors-sum)))))

(fact "Check the amicable? method returns correct number"
                                        ; mock test
  (amicable? 10) => true
  (provided
    (all-divisors-bl 10) => [1 3]
    (all-divisors-bl 4) =>  [1 4 5])
  (amicable? 1) => false
  (amicable? 10) => false
  (amicable? 284) => true
  (amicable? 220) => true)

(defn amicable-numbers-under "Determine the amicable numbers from an interval"
  [borne]
  (if (neg? borne) []
      (filter #(not= nil %)
              (map (fn [num]
                     (if (amicable? num) num))
                   (range 1 (inc borne))))))

;.;. A clean boundary between useful abstractions and the grubby code that
;.;. touches the real world is always a good thing. -- Ron Jeffries
(fact "Check the amicable numbers generating sequence."
                                        ; mock test top-down tdd
  (amicable-numbers-under 5) => [1 2 4 5]
  (provided
    (amicable? 1) => true
    (amicable? 2) => true
    (amicable? 3) => false
    (amicable? 4) => true
    (amicable? 5) => true)
                                        ; limit use case
  (amicable-numbers-under -1) => []
  (amicable-numbers-under 10) => []
  (amicable-numbers-under 1000) => [220 284]
  (amicable-numbers-under 10000) => [220 284])

(defn sum-amicable-numbers "Compute the sum of amicable numbles given an interval"
  [borne]
  (reduce + (amicable-numbers-under 10000)))

(fact "Check that the sum of amicable numbers for an interval is ok."
  (sum-amicable-numbers 10000) => 504
  (provided (amicable-numbers-under 10000) => [220 284]))

(future-fact
  (sum-amicable-numbers 10000) => 31626)
