(ns my-project-euler-lab.core4
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;; problem 4

;; A palindromic number reads the same both ways. 
;; The largest palindrome made from the product of two
;; 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit
;; numbers.

(defn pal? "Is the sequence given a palindrome?"
  [v]
  (= (seq v) (reverse v)))

(fact
  (pal? [1 2 3]) => falsey
  (pal? [1 2 3 4]) => falsey
  (pal? [1 2 2 1]) => truthy
  (pal? ["1" "2" "2" "1"]) => truthy
  (pal? ["1" "2" "2" "1" "2" "1"]) => falsey
  (pal? ["1" "2" "2" "2" "2" "1"]) => truthy
  (pal? [\1 \2 \2 \1]) => truthy
  (pal? [\1 \2 \2 \2]) => falsey
  (pal? [1 2 3 2 1]) => truthy)

(defn gen-all-mult "Create the sequence of product from all the elements in the sequence 2x2. The product are filtered to not be above the limit."
  [seq0 limit]
  (distinct 
   (for [x seq0 y seq0 :when (<= (* x y) limit)]  (* x y))))

;.;. The biggest reward for a thing well done is to have done it. --
;.;. Voltaire
(fact
  (gen-all-mult (range 1 11) 100)
  => (contains [10 20 30 40 50 60 70 80 90 100 9 18 27 36 45 54 63 72 81 8 16 24 32 48 56 64 7 14 21 28 35 42 49 6 12 5 15 25 4 3 2 1] :in-any-order :gaps-ok)
    (gen-all-mult (range 1 11) 10)
  => (contains [10 9 8 7 6 5 4 3 2 1] :in-any-order :gaps-ok))

(defn max-pal-6 "Find the largest palindrome"
  []
  (reduce
   max
   (filter
    #(and (not= nil %) (pal? (str %)))
    (gen-all-mult (range 100 1000) 1000000))))

(fact (max-pal-6) => 906609)

