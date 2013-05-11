(ns euler-lab.core26
  (:use [midje.sweet])
  (:require [clojure.string :as s]
            [clojure.tools.trace :as t]))

(->> (for [x (range 1 1000)]
       [x (double (/ 1 x))])
     (map (fn [[x s]] [x (str s)]))
     (map (fn [[x s]] [x (second (s/split s #"\."))]))
     (map (fn [[x s]]
            [x (first (reduce (fn [[sum p] e] (if (not= p e) [(+ sum 1) e] [sum e])) [0 -1] s))]))
     (group-by second))
