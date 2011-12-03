(ns my-project-euler-lab.core-pb-4
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

                                        ; problem 4

;; A palindromic number reads the same both ways. 
;; The largest palindrome made from the product of two
;; 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit
;; numbers.

(defn pal? "Is the sequence given a palindrome?"
  [v]
  (loop [seq v]
    (or (<= (count seq) 1)
        (and (= (first seq) (last seq))
             (recur (butlast (rest seq)))))))

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
   (mapcat
    (fn [e] (map
            (fn [s] (let [pdt (* s e)] (when (<= pdt limit) pdt)))
            seq0))
    seq0)))

(fact
  (gen-all-mult (range 1 11) 100)
  => (contains [10 20 30 40 50 60 70 80 90 100 9 18 27 36 45 54 63 72 81 8 16 24 32 48 56 64 7 14 21 28 35 42 49 6 12 5 15 25 4 3 2 1] :in-any-order)
    (gen-all-mult (range 1 11) 10)
  => (contains [10 9 8 7 6 5 4 3 2 1] :in-any-order))

(defn max-pal-6 "Find the largest palindrome"
  []
  (reduce
   max
   (filter
    #(and (not= nil %) (pal? (num-digits-into-vec %)))
    (gen-all-mult (range 100 1000) 1000000))))

;.;. There is an inevitable reward for good deeds. -- Ming Fu Wu
(fact (max-pal-6) => 906609)

