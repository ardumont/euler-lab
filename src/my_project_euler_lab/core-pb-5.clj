(ns my-project-euler-lab.core-pb-5
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

                                        ; problem 5

;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
(fact (reduce + (map #(rem 2520 %) (range 2 11))) => 0)

;What is the smallest positive number that is evenly divisible by all
;of the numbers from 1 to 20?

; brute force

(defn euler-5-ok? "Can the number be divided by each of the numbers from 1 to borne without any remainder?"
  [num borne]
  (not (some #(not= 0 %) (map #(rem num %) (range 2 (inc borne))))))

(fact
  (euler-5-ok? 2520 10) => true
  (euler-5-ok? 2521 10) => false
 )

(defn find-smallest-ok-euler-5 "Find the smallest number that check the predicate euler-5-ok?"
  [inc start range]
  (first (take 1 (drop-while #(not (euler-5-ok? % range)) (iterate #(+ inc %) start))))
  )

;.;. The right word may be effective, but no word was ever as effective as
;.;. a rightly timed pause. -- Twain
(fact(find-smallest-ok-euler-5 2 10 10) => 2520)
;(fact (find-smallest-ok-euler-5 2 20 20) => 232792560)



