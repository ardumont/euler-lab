(ns my-project-euler-lab.core-pb-5
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; problem 5

;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
(fact (reduce + (map #(rem 2520 %) (range 1 10))) => 0)

;What is the smallest positive number that is evenly divisible by all
;of the numbers from 1 to 20?

