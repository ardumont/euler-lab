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

(fact
  (num-digits-into-vec 100) => [1 0 0]
  (num-digits-into-vec 100256789) => [1 0 0 2 5 6 7 8 9]
  )

;.;. Any intelligent fool can make things bigger, more complex, and more
;.;. violent. It takes a touch of genius -- and a lot of courage -- to move
;.;. in the opposite direction. -- Schumacher
(fact (reduce + (num-digits-into-vec (expt 2 1000))) => 1366)

