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

(def work-vector [0 1 2 3 4 5 6 7 8 9])

(defn permut "Compute all the possible permutations a the given vector"
  [v]
  (let [count-v (count v)]
    (cond (<= count-v 1) v
          (= 2 count-v) [v [(second v) (first v)]]
          (= 3 count-v) (concat  (map #(concat [(nth v 0)] %) (permut [(nth v 1) (nth v 2)]))
                                 (map #(concat [(nth v 1)] %) (permut [(nth v 2) (nth v 0)]))
                                 (map #(concat [(nth v 2)] %) (permut [(nth v 0) (nth v 1)])))
          :else v)))

(fact
  (permut [0 1]) => [[0 1] [1 0]])

;.;. Any intelligent fool can make things bigger, more complex, and more
;.;. violent. It takes a touch of genius -- and a lot of courage -- to move
;.;. in the opposite direction. -- Schumacher
(fact
  (permut [0 1 2]) => [[0 1 2] [0 2 1] [1 2 0] [1 0 2] [2 0 1] [2 1 0]])


(println "--------- END OF PB 24 ----------" (java.util.Date.))
