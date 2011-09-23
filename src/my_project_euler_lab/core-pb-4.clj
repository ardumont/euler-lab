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

