(ns my-project-euler-lab.core-pb-9
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a^2 + b^2 = c^2

;For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.

