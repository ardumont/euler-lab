(ns euler-lab.core15
  "Problem 15 - http://projecteuler.net/problem=15
Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.
How many routes are there through a 20×20 grid?"
  (:require [midje.sweet     :as m]
            [euler-lab.combi :as combi]))

;1x1
;a b => a b | a
;c d      d | c d
; 2 routes

;2x2
;a b c  => a     | a     | a     | a b   | a b   | a b c
;e f g     e     | e f   | e f g |   f   |   f g |     g
;i j k     i j k |   j k |     k |   j k |     k |     k
;=> 6 routes
; a b => a b | a   => 2 routes * 2 routes from going from f to i
; e f      f | e f
; +1 route from the second matrix b c f g
; +1 route from the third matrix e f g h
; + 0 route from the last because no more can be done
; = 2*P(1x1)+1+1+0
; 6 feuilles
;              a
;          b       e
;        c   f   f   i
;        g  g j j g  j
;        k  k k k k  k

;3x3
; a b c d =>  a b c  => a     | a     | a     | a b   | a b   | a b c
; e f g h     e f g     e     | e f   | e f g |   f   |   f g |     g
; i j k l     g h i     i j k |   j k |     k |   j k |     k |     k
; m n o p               OK    | OK    | OK    | OK    |  OK   | OK
;             and
;             k l    => k l | k
;             o p         p | o p
; so there is 6*2=12 ways with the first 2x2 matrices from going from
; a to p
; [a e i j k l p] [a e i j k o p]
; [a e f j k l p] [a e f j k o p]
; [a e f g k l p] [a e f g k o p]
; [a b f j k l p] [a b f j k o p]
; [a b f g k l p] [a b f g k o p]
; [a b c g k l p] [a b c g k o p]

; a b c d =>  b c d  => b     | b     | b     | b c   | b c   | b c d
; e f g h     f g h     f     | f g   | f g h |   g   |   g h |     h
; i j k l     j k l     j k l |   k l |     l |   k l |     l |     l
; m n o p
; and one way to go from a to b [a b]
; and one way to go from l to p [l p]
; [a b f j k l p] -> KO, already present in the first matrice
; [a b f g k l p] -> KO
; [a b f g h l p] -> OK
; [a b c g k l p] -> KO
; [a b c g h l p] -> OK
; [a b c d h l p] -> OK
; => 3 OK

; a b c d =>        => etc...
; e f g h     e f g
; i j k l     i j k
; m n o p     m n o
; the same way, we count 6 but 3 are already counted

; a b c d =>        => etc...
; e f g h     f g h
; i j k l     j k l
; m n o p     n o p
; everything is already counted except for the 2 routes
; [a b f j n o p] and [a e f g h l p]
;=> 12 + 3 + 3 + 2 = 20 = 2*P(2x2)+ 1/2*P(2x2)+1/2*P(2x2)+2 = 3*P(2x2)+2
; (2+1/2+1/2+1/3)P(2x2) = 10/3P(2x2)

;(m/fact (intersection #{[1 2 3] [4 5 6]} #{[1 2 3]}) => #{[1 2 3]})
;(m/fact (difference #{[1 2 3] [4 5 6]} #{[1 2 3]})) => #{[4 5 6]}
;(m/fact (difference #{[1 2 3] [4 5 6]} #{[1 2 3] [4 5 6]})) => #{}
;(m/fact (difference #{[1 2 3]} #{[1 2 3] [4 5 6]})) => #{}
;(m/fact (union #{[1 2 3]} #{[1 2 3] [4 5 6]})) => #{[1 2 3] [4 5 6]}

(m/fact
  (combi/combi 2 1) => 2
  (combi/combi 4 2) => 6
  (combi/combi 6 3) => 20
  (combi/combi 8 4) => 70)

(m/future-fact
 (combi/combi 40 20) => 137846528820)
