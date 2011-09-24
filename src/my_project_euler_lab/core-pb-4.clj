(ns my-project-euler-lab.core-pb-4
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; problem 4

;A palindromic number reads the same both ways. 
;The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;Find the largest palindrome made from the product of two 3-digit
;numbers.

;((fact palimdromic-num 2) => [91 99])

(defn pal? "Is the string given a palindrome?"
  [v]
  (loop [seq v]
    (or (<= (count seq) 1)
        (and (= (first seq) (last seq)) (recur (butlast (rest seq)))) 
        )
    )
  )

;.;. Work is either fun or drudgery. It depends on your attitude. I like
;.;. fun. -- Barrett
(fact
  (pal? [1 2 3]) => false
  (pal? [1 2 3 4]) => false
  (pal? [1 2 2 1]) => true
  (pal? ["1" "2" "2" "1"]) => true
  (pal? ["1" "2" "2" "1" "2" "1"]) => false
  (pal? ["1" "2" "2" "2" "2" "1"]) => true
  (pal? [1 2 3 2 1]) => true
  )
