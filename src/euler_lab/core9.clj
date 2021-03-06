 (ns euler-lab.core9
   "Problem 9 - http://projecteuler.net/problem=9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

1. a < b < c
2. a^2 + b^2 = c^2
3. a+b+c=1000 <=> a+b=1000-c"
  (:require [midje.sweet :as m]))

(defn is-triplet-ok? "Is the triplet is ok according to the hypothesis of the problem?"
  [a b c]
  (and (< a b c) (== 1000 (+ a b c)) (== (+ (* a a) (* b b)) (* c c))))

(defn find-triplet-ok-bf "Find the triplet that correspond to the hypothesis of the problem."
  [vec]
  (loop [nnat-sq0 vec nnat-sq1 (filter #(< (first nnat-sq0) %) vec) cnt (count nnat-sq1)]
    (if (== 1 (count nnat-sq0))
      []
      (if (== 1 cnt); recur over nnat-sq1
        (let [rest-nnat0 (rest nnat-sq0) rest-nnat1 (filter #(< (first rest-nnat0) %) rest-nnat0)]
          (recur rest-nnat0 rest-nnat1 (count rest-nnat1))); end nnat-sq1
        (let [a (first nnat-sq0) b (first nnat-sq1) c (- 1000 a b)]
          (if (is-triplet-ok? a b c)
            [a b c]
            (recur nnat-sq0 (rest nnat-sq1) (dec cnt))))))))

(m/fact
;;  (find-triplet-ok-bf (range 0 (Math/sqrt 1000))) => [200 375 425]
  (find-triplet-ok-bf (range 0 1000)) => [200 375 425])

(m/fact (reduce * (find-triplet-ok-bf (range 0 1000))) => 31875000)
