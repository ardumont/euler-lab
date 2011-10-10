(ns my-project-euler-lab.core-pb-16
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math)
  ;(:use [clojure.set                :only [intersection difference
                                        ;union]])
  (:use my-project-euler-lab.combi)
  )
;2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;What is the sum of the digits of the number 2^1000?

(fact (expt 2 1000) => 10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376)

(defn num-digits-into-vec "Transform big integer into a vector of digits."
  [num]
  (loop [n num acc nil]
    (if (zero? n)
      (vec acc)
      (recur (quot n 10) (cons (rem n 10) acc)))))

;.;. A clean boundary between useful abstractions and the grubby code that
;.;. touches the real world is always a good thing. -- Ron Jeffries
(fact
  (num-digits-into-vec 100) => [1 0 0]
  (num-digits-into-vec 100256789) => [1 0 0 2 5 6 7 8 9]
  (num-digits-into-vec 10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376) => [1 0 7 1 5 0 8 6 0 7 1 8 6 2 6 7 3 2 0 9 4 8 4 2 5 0 4 9 0 6 0 0 0 1 8 1 0 5 6 1 4 0 4 8 1 1 7 0 5 5 3 3 6 0 7 4 4 3 7 5 0 3 8 8 3 7 0 3 5 1 0 5 1 1 2 4 9 3 6 1 2 2 4 9 3 1 9 8 3 7 8 8 1 5 6 9 5 8 5 8 1 2 7 5 9 4 6 7 2 9 1 7 5 5 3 1 4 6 8 2 5 1 8 7 1 4 5 2 8 5 6 9 2 3 1 4 0 4 3 5 9 8 4 5 7 7 5 7 4 6 9 8 5 7 4 8 0 3 9 3 4 5 6 7 7 7 4 8 2 4 2 3 0 9 8 5 4 2 1 0 7 4 6 0 5 0 6 2 3 7 1 1 4 1 8 7 7 9 5 4 1 8 2 1 5 3 0 4 6 4 7 4 9 8 3 5 8 1 9 4 1 2 6 7 3 9 8 7 6 7 5 5 9 1 6 5 5 4 3 9 4 6 0 7 7 0 6 2 9 1 4 5 7 1 1 9 6 4 7 7 6 8 6 5 4 2 1 6 7 6 6 0 4 2 9 8 3 1 6 5 2 6 2 4 3 8 6 8 3 7 2 0 5 6 6 8 0 6 9 3 7 6]
  )

; solution to the problem
(fact (reduce + (num-digits-into-vec (expt 2 1000))) => 1366)

