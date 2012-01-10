(ns my-project-euler-lab.combi
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math))

(defn factorial "Factorial method"
  [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

;.;. Code you'd be proud to give your mom to show off on the fridge. --
;.;. Mike Cohn
(fact
  (factorial 1) => 1
  (factorial 2) => 2
  (factorial 3) => 6
  (factorial 4) => 24
  (factorial 5) => 120
  (factorial 6) => 720
  (factorial 7) => 5040)

(defn arrangement "Arrangement"
  [n k]
  (if (< n k)
    0
    (/ (factorial n) (factorial (- n k))))
  )

(fact
  (arrangement 0 1) => 0
  (arrangement 0 0) => 1
  (arrangement 5 2) => 20
  (arrangement 5 0) => 1
  (arrangement 5 5) => 120
)

(defn combi "Number of combinations"
  [n k]
  (if (< n k)
    0
    (/ (factorial n) (* (factorial k) (factorial (- n k)))))
  )

(fact
  (combi 0 1) => 0
  (combi 0 0) => 1
  (combi 5 2) => 10
  (combi 10 0) => 1
  (combi 10 10) => 1)
