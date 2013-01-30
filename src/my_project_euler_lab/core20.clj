(ns my-project-euler-lab.core20
  (:use [midje.sweet]
        [my-project-euler-lab.combi :only [factorial]]
        [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;; n! means n × (n − 1) × ... × 3 × 2 × 1

;; For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;; and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

;; Find the sum of the digits in the number 100!

(defn count-digit-from-factorial "Count the sum of number from the factorial number"
  [n]
  (if (< n 0)
    0
    (reduce + (num-digits-into-vec (factorial n)))))

;.;. A journey of a thousand miles begins with a single step. --
;.;. @alanmstokes
(fact
  (count-digit-from-factorial -10) => 0
  (count-digit-from-factorial 10) => 27)

(future-fact
 (count-digit-from-factorial 100)) => 648
