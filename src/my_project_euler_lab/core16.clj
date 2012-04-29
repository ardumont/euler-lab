(ns my-project-euler-lab.core16
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;; What is the sum of the digits of the number 2^1000?

(fact (clojure.contrib.math/expt 2 1000) => 10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376)

(fact (reduce + (num-digits-into-vec (clojure.contrib.math/expt 2 1000))) => 1366)

