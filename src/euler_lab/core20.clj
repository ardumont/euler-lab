(ns euler-lab.core20
  "Problem 20 - http://projecteuler.net/problem=20
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
"
  (:require [midje.sweet     :as m]
            [euler-lab.combi :as combi]
            [euler-lab.utils :as utils]))

(defn count-digit-from-factorial "Count the sum of number from the factorial number"
  [n]
  (if (< n 0)
    0
    (reduce + (utils/num-digits-into-vec (combi/! n)))))

(m/fact
  (count-digit-from-factorial -10) => 0
  (count-digit-from-factorial 10)  => 27)

(m/future-fact
 (count-digit-from-factorial 100) => 648)
