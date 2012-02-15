(ns my-project-euler-lab.core-pb-14
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]]))

;; The following iterative sequence is defined for the set of positive integers:
;; n → n/2 (n is even)
;; n → 3n + 1 (n is odd)
;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. 
;; Although it has not been proved yet (Collatz Problem), it is thought
;; that all starting numbers finish at 1.

;; Which starting number, under one million, produces the longest chain?

;; NOTE: Once the chain starts the terms are allowed to go above one million.

(defn seq-euler-14 "Sequence pb 14"
  [n]
  (loop [curr n seq-res []]
    (if (= 1 curr)
      (conj seq-res 1)
      (if (even? curr)
        (recur (/ curr 2) (conj seq-res curr))
        (recur (+ (* 3 curr) 1) (conj seq-res curr))))))

(fact
  (seq-euler-14 13) => [13 40 20 10 5 16 8 4 2 1]
  (seq-euler-14 20) => [20 10 5 16 8 4 2 1]
  (seq-euler-14 1024) => [1024 512 256 128 64 32 16 8 4 2 1]
  )

(defn seq-euler-14-count "Sequence pb 14 but only for counting the nb of elements"
  [n]
  (loop [curr n cnt 1]
    (if (= 1 curr)
      cnt
      (if (even? curr)
        (recur (/ curr 2) (inc cnt))
        (recur (+ (* 3 curr) 1) (inc cnt))))))

(fact
  (seq-euler-14-count 13) => 10
  (seq-euler-14-count 20) => 8
  (seq-euler-14-count 1024) => 11
  (seq-euler-14-count (expt 2 20)) => 21
  ) 

; seq which begins with a power of 2 will be the smallest

; if n is odd => n=2p+1, for all p in {1,(n-1)/2}
;             => 3n+1 = 3(2p+1) + 1 = 6p + 4
; then 3n+1 is even

; if n is even => n = 2p for all p in {1,n/2}
;              => n/2 = 2p/2 = p
; then n/2 can be even or odd

(defn find-greatest-seq-below-start "Find the greatest sequence with the sequence starting from a number below 1 000 000"
  [starting-from end]
  (loop [cnt starting-from maxi 1 start starting-from]; maxi -> max result from
                                        ; all computations, start ->
                                        ; counter from which the max is
    (if (<= cnt end)
      [start maxi]
      (let [seq-euler-res (seq-euler-14-count cnt)]
        (if (< maxi seq-euler-res)
          (recur (dec cnt) seq-euler-res cnt)
          (recur (dec cnt) maxi start)))))
  )

(fact
;    (find-greatest-seq-below-start 1000000 500000) => [837799 525]
    )

;my-project-euler-lab.core-pb-14> (time (find-greatest-seq-below-start
;1000000 500000))
;"Elapsed time: 9739.327681 msecs"
;[837799 525]

;my-project-euler-lab.core-pb-14> (time (find-greatest-seq-below-start
;1000000 1))
;"Elapsed time: 18319.081637 msecs"
;[837799 525]
