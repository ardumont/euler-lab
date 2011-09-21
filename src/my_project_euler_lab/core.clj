(ns my-project-euler-lab.core
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

; If we list all the natural numbers below 10 that are multiples of 3
; or 5, we get 3, 5, 6 and 9. 
; The sum of these multiples is 23.
; Find the sum of all the multiples of 3 or 5 below 1000.

(defn nn-mult-3-5 "Return the list of the natural numbers multiple of 3 or 5 below n"
  [n]
  (filter #(or (= 0 (rem % 3)) (= 0 (rem % 5))) (range 1 n)) 
  )

(fact (nn-mult-3-5 10) => [3 5 6 9])
;.;. Intellectual 'work' is misnamed; it is a pleasure, a dissipation, and
;.;.                                ; is its own highest reward. -- Twain
(fact (reduce + (nn-mult-3-5 10)) => 23)

;my-project-euler-lab.core> (reduce + (nn-mult-3-5 1000))
; 233168
