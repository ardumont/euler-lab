(ns my-project-euler-lab.core-pb-20
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.combi :only [factorial]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;n! means n × (n − 1) × ... × 3 × 2 × 1
;
;For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
;
;Find the sum of the digits in the number 100!

(defn count-digit-from-factorial "Count the sum of number from the factorial number"
  [n]
  (if (< n 0)
    0
    (reduce + (num-digits-into-vec (factorial n))))
  )

;.;. A journey of a thousand miles begins with a single step. --
;.;. @alanmstokes
(fact
  (count-digit-from-factorial -10) => 0
  (count-digit-from-factorial 10) => 27
  (count-digit-from-factorial 100) => 648
  )
