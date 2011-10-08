(ns my-project-euler-lab.core-pb-14
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

;The following iterative sequence is defined for the set of positive integers:
;
;n → n/2 (n is even)
;n → 3n + 1 (n is odd)
;
;Using the rule above and starting with 13, we generate the following sequence:
;13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;
;It can be seen that this sequence (starting at 13 and finishing at 1)
;contains 10 terms. 
;Although it has not been proved yet (Collatz Problem), it is thought
;that all starting numbers finish at 1.

;Which starting number, under one million, produces the longest chain?
;
;NOTE: Once the chain starts the terms are allowed to go above one million.

(defn seq-euler-14 "Sequence pb 14"
  [n]
  (loop [curr n seq-res []]
    (if (= 1 curr)
      (conj seq-res 1)
      (if (even? curr)
        (recur (/ curr 2) (conj seq-res curr))
        (recur (+ (* 3 curr) 1) (conj seq-res curr))))))

(fact
  (seq-euler-14 13) => [13 40 20 10 5 16 8 4 2 1]
  )
