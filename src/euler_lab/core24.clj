(ns euler-lab.core24
  "Problem 24 - http://projecteuler.net/problem=24
A permutation is an ordered arrangement of objects.
For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
If all of the permutations are listed numerically or
alphabetically, we call it lexicographic order. The lexicographic
permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1,
2, 3, 4, 5, 6, 7, 8 and 9?"
  (:use [midje.sweet]))

(defn circular-permut "Make a circular permutation from a vector."
  [v]
  (conj
   (subvec v 1 (count v)) (first v)))

(fact "Test the circular permutation"
  (circular-permut [0 1 2])   => [1 2 0]
  (circular-permut [0 1 2])   => vector?
  (circular-permut [1 2 0])   => [2 0 1]
  (circular-permut [2 0 1])   => [0 1 2]
  (circular-permut [2 0 1 9]) => [0 1 9 2])

(defn all-circular-permut "Compute all circular permutations of a vector and sort them."
  [v]
  (loop [curr (dec (count v)) acc [v]]
    (if (zero? curr)
      (sort acc)
      (recur (dec curr) (conj acc (circular-permut (last acc)))))))

(fact "Mock - Compute all circular permutations of a vector"
  (all-circular-permut [0 1 2]) => [[0 1 2] [1 2 0] [2 0 1]]
  (provided
    (circular-permut [0 1 2])   => [1 2 0]
    (circular-permut [1 2 0])   => [2 0 1]))

(fact "ITest - Compute all circular permutations of a vector"
  (all-circular-permut [0 1 2])   => [[0 1 2] [1 2 0] [2 0 1]]
  (all-circular-permut [0 1 2 3]) => [[0 1 2 3] [1 2 3 0] [2 3 0 1] [3 0 1 2]])

(defn permut "Compute all the permutations possibles from a vector."
  [v]
  (let [count-v (count v)]
    (cond (<= count-v 1) v
          (= 2 count-v) (lazy-seq (let [[x y] v]
                                    (if (< x y)
                                      [(str x y) (str y x)]
                                      [(str y x) (str x y)])))
          :else
          (lazy-seq (reduce concat
                            (map (fn [v-circ-permut]
                                   (let [[fst & more] v-circ-permut]
                                     (map (fn [p] (str fst p)) (permut more))))
                                 (all-circular-permut (vec v))))))))

(fact "Itest - Test a permutation generation from a vector of number"
  (permut [0 1])       => (just ["01" "10"])
  (permut [0 1 2])     => (just ["012" "021" "102" "120" "201" "210"])
  (permut [0 1 2 3])   => (just ["0123" "0132" "0213" "0231" "0312" "0321"
                               "1023" "1032" "1203" "1230" "1302" "1320"
                               "2013" "2031" "2103" "2130" "2301" "2310"
                               "3012" "3021" "3102" "3120" "3201" "3210"])
  (permut [0 1 2 3 4]) => (just ["01234" "01243" "01324" "01342" "01423" "01432" "02134" "02143" "02314" "02341" "02413" "02431" "03124" "03142" "03214" "03241" "03412" "03421" "04123"
                                 "04132" "04213" "04231" "04312" "04321"
                                 "10234" "10243" "10324" "10342" "10423" "10432" "12034" "12043" "12304" "12340" "12403" "12430" "13024" "13042" "13204" "13240" "13402" "13420" "14023"
                                 "14032" "14203" "14230" "14302" "14320"
                                 "20134" "20143" "20314" "20341" "20413" "20431" "21034" "21043" "21304" "21340" "21403" "21430" "23014" "23041" "23104" "23140" "23401" "23410" "24013"
                                 "24031" "24103" "24130" "24301" "24310"
                                 "30124" "30142" "30214" "30241" "30412" "30421" "31024" "31042" "31204" "31240" "31402" "31420" "32014" "32041" "32104" "32140" "32401" "32410" "34012"
                                 "34021" "34102" "34120" "34201" "34210"
                                 "40123" "40132" "40213" "40231" "40312" "40321" "41023" "41032" "41203" "41230" "41302" "41320" "42013" "42031" "42103" "42130" "42301" "42310" "43012"
                                 "43021" "43102" "43120" "43201" "43210"]))

(defn find-rth-permut "Find the rth permutations from the vector v"
  [r v]
  (let [p (take r (permut v))]
    (last p)))

(future-fact "The call to solve the problem 24."
 (find-rth-permut 1000000 [0 1 2 3 4 5 6 7 8 9]) => "2783915460"); first time -> out of memory error ; second time: use of lazy-seq and bam, it worked!

; i need to sioux. 10! is too heavy in memory.
; I think along this line.
; 9! => 362880 and the algorithm works for this.

; so what we know:
; - the first 362880 permutations of 0123456789 begins with 0
; - the second  362880 permutations of 0123456789 begins with 1
; - the third 362880 permutations of 0123456789 begins with 2

; and guess what (rem 1000000 362880) = 274240
; so we need to find the 274240th permutations from the set 345678901
; to solve the problems and concatenate a 2 before this result to have
; the solution of the problem 24.

(fact "The tweaked call to solve the problem 24."
  (str "2" (find-rth-permut 274240 [3 4 5 6 7 8 9 0 1])) => "2783915460")
