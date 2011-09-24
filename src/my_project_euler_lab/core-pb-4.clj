(ns my-project-euler-lab.core-pb-4
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))

                                        ; problem 4

;A palindromic number reads the same both ways. 
;The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;Find the largest palindrome made from the product of two 3-digit
;numbers.

;((fact palimdromic-num 2) => [91 99])

(defn pal? "Is the string given a palindrome?"
  [v]
  (loop [seq v]
    (or (<= (count seq) 1)
        (and (= (first seq) (last seq)) (recur (butlast (rest seq)))) 
        )
    )
  )

(fact
  (pal? [1 2 3]) => false
  (pal? [1 2 3 4]) => false
  (pal? [1 2 2 1]) => true
  (pal? ["1" "2" "2" "1"]) => true
  (pal? ["1" "2" "2" "1" "2" "1"]) => false
  (pal? ["1" "2" "2" "2" "2" "1"]) => true
  (pal? [1 2 3 2 1]) => true
  )


;(map #(* 100 %) (take 900 (iterate inc 100)))
;(filter #() (test-gen-all-mult (take 900(iterate inc 100)))))

(defn test-gen-all-mult "Product of all 100-999 numbers"
  [vec]
  (loop [n (count vec) v vec acc nil]
    (if (zero? n)
      (distinct acc)
      (recur (dec n) (rest v) (concat (map #(* (first v) %) vec) acc))
      )
    )
  )

(defn max-pal-4 "Find the largest palindrome"
  []
  (last (sort (filter #(and (>= (/ % 1000) 1) (pal? (re-seq #"[\d]" (.toString %))))
                  (test-gen-all-mult (take 90 (iterate inc 10)))))))

(fact (max-pal-4) => 9009)

(defn max-pal-6 "Find the largest palindrome"
  []
  (last (sort (filter #(and (>= (/ % 100000) 1) (pal? (re-seq #"[\d]" (.toString %))))
                  (test-gen-all-mult (take 900 (iterate inc 100)))))))

;.;. The reward of a thing well done is to have done it. -- Emerson
(fact (max-pal-6) => 906609)
