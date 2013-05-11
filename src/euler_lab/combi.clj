(ns euler-lab.combi
  (:require [midje.sweet :as m]))

(defn ! "Factorial method"
  [n]
  (loop [a 1 c n]
    (if (= 0 c)
      a
      (recur (* a c) (dec c)))))

(m/fact
  (! 1) => 1
  (! 2) => 2
  (! 3) => 6
  (! 4) => 24
  (! 5) => 120
  (! 6) => 720
  (! 7) => 5040)

(defn arrangement "Arrangement"
  [n k]
  (if (< n k)
    0
    (/ (! n) (! (- n k)))))

(m/fact
  (arrangement 0 1) => 0
  (arrangement 0 0) => 1
  (arrangement 5 2) => 20
  (arrangement 5 0) => 1
  (arrangement 5 5) => 120)

(defn combi "Number of combinations"
  [n k]
  (if (< n k)
    0
    (/ (! n) (* (! k) (! (- n k))))))

(m/fact
  (combi 0 1) => 0
  (combi 0 0) => 1
  (combi 5 2) => 10
  (combi 10 0) => 1
  (combi 10 10) => 1)
