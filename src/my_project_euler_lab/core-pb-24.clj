(ns my-project-euler-lab.core-pb-24
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.set :only [difference union]] )
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.combi]))

;A permutation is an ordered arrangement of objects.
;For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
;If all of the permutations are listed numerically or
;alphabetically, we call it lexicographic order. The lexicographic
;permutations of 0, 1 and 2 are:

;012   021   102   120   201   210

;What is the millionth lexicographic permutation of the digits 0, 1,
;2, 3, 4, 5, 6, 7, 8 and 9?

; solution
; there are 10! solutions; 3628800.

(unfinished)

(def work-vector [0 1 2 3 4 5 6 7 8 9])

(defn circular-permut "Make a circular permutation from a vector."
  [v]
  (conj
   (subvec v 1 (count v)) (first v) ))

(fact "Test the circular permutation"
  (circular-permut [0 1 2]) => [1 2 0]
  (circular-permut [0 1 2]) => vector?
  (circular-permut [1 2 0]) => [2 0 1]
  (circular-permut [2 0 1]) => [0 1 2]
  (circular-permut [2 0 1 9]) => [0 1 9 2])

(defn all-circular-permut "Compute all circular permutations of a vector."
  [v]
  (loop [curr (dec (count v)) acc [v]]
    (if (zero? curr)
      acc
      (recur (dec curr) (conj acc (circular-permut (last acc)))))))

(fact "Mock - Compute all circular permutations of a vector"
  (all-circular-permut [0 1 2]) => [[0 1 2] [1 2 0] [2 0 1]]
  (provided
    (circular-permut [0 1 2]) => [1 2 0]
    (circular-permut [1 2 0]) => [2 0 1]))

(fact "ITest - Compute all circular permutations of a vector"
  (all-circular-permut [0 1 2]) => [[0 1 2] [1 2 0] [2 0 1]]
  (all-circular-permut [0 1 2]) => vector?
  (all-circular-permut [0 1 2 3]) => [[0 1 2 3] [1 2 3 0] [2 3 0 1] [3 0 1 2]])

(defn permut "Compute all the permutations possibles from a vector."
  [v]
  (let [count-v (count v)]
    (cond (<= count-v 1) v                         ; nothing to do
          (= 2 count-v) (let [[x y] v] [[x y] [y x]]) ; only 2 permutations
          :else
          (reduce concat
                  (map (fn [v-circ-permut]
                         (let [[fst & more] v-circ-permut]
                              (map (fn [p] (concat [fst] p)) (permut more))))
                       (all-circular-permut (vec v)))))))

;.;. The biggest reward for a thing well done is to have done it. --
;.;. Voltaire
(fact "Itest - Test a permutation generation from a vector of number"
  (permut [0 1]) => [[0 1] [1 0]]
  (permut [0 1 2]) => [[0 1 2] [0 2 1] [1 2 0] [1 0 2] [2 0 1] [2 1 0]]
  (permut [0 1 2 3]) => ['(0 1 2 3) '(0 1 3 2) '(0 2 3 1) '(0 2 1 3) '(0 3 1 2) '(0 3 2 1)
                         '(1 2 3 0) '(1 2 0 3) '(1 3 0 2) '(1 3 2 0) '(1 0 2 3) '(1 0 3 2)
                         '(2 3 0 1) '(2 3 1 0) '(2 0 1 3) '(2 0 3 1) '(2 1 3 0) '(2 1 0 3)
                         '(3 0 1 2) '(3 0 2 1) '(3 1 2 0) '(3 1 0 2) '(3 2 0 1) '(3 2 1 0)])

(println "--------- END OF PB 24 ----------" (java.util.Date.))

