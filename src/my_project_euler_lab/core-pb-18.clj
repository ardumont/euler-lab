(ns my-project-euler-lab.core-pb-18
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
;    3
;   7 4
;  2 4 6
; 8 5 9 3
;
;That is, 3 + 7 + 4 + 9 = 23.
;
;Find the maximum total from top to bottom of the triangle below:
;
;                  75
;                 95 64
;               17 47 82
;              18 35 87 10
;             20 04 82 47 65
;            19 01 23 75 03 34
;           88 02 77 73 07 63 67
;          99 65 04 28 06 16 70 92
;         41 41 26 56 83 40 80 70 33
;        41 48 72 33 47 32 37 16 94 29
;       53 71 44 65 25 43 91 52 97 51 14
;      70 11 33 28 77 73 17 78 39 68 17 57
;     91 71 52 38 17 14 91 43 58 50 27 29 48
;   63 66 04 68 89 53 67 30 73 16 69 87 40 31
;  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
;
;NOTE: As there are only 16384 routes, it is possible to solve this
;    problem by trying every route. However, Problem 67, is the same
;    challenge with a triangle containing one-hundred rows
; it cannot be solved by brute force, and requires a clever method! ;o)

; how to represent this tree?

; second: {[0 0] {:value 3 :childs {[1 0] [1 1]}},
;          [1 0] {:value 7 :childs {[2 0] [2 1]}} 
;          [1 1] {:value 4 :childs {[2 1] [2 2]}}
;          [2 0] {:value 2 :childs {[3 0] [3 1]}}
;          [2 1] {:value 4 :childs {[3 1] [3 2]}}
;          [2 2] {:value 6 :childs {[3 2] [3 3]}}
;          [3 0] {:value 8 :childs {[]}}
;          [3 1] {:value 5 :childs {[]}}
;          [3 2] {:value 9 :childs {[]}}
;          [3 3] {:value 3 :childs {[]}}

; fixme: there is some nodes which have children and they do not need to exist
; find some way to not have them

(defn depth "Compute the depth of a tree from a sequence"
  [s]
  (let [count-s (count s)
        map-tri (map #(* % (inc %) 1/2) (take count-s (iterate inc 1)))]
    (loop [curr 0 map-triangle map-tri]
      (if (= nil (first map-triangle))
        nil
        (if (= count-s (first map-triangle))
          (inc curr)
          (recur (inc curr) (rest map-triangle))))
      )
    )
  )

(fact
  (depth [1]) => 1
  (depth [10 3 3]) => 2
  ; not a triangle this one!
  (depth [10 3 3 1 2 3]) => 3
  (depth [10 3 3 1 2 3 4]) => nil
  )

(defn coordinates "Make a vector of coordinates"
  [n]
  (cond (zero? n) []
        (= 1 n) [[0 0]]
        :else (let [n-minus-1 (dec n)]
                (loop [acc [] cnt 0 x 0]
                  (if (<= n cnt)
                    acc
                    (let [map-coord (map #(vec [x %]) (range 0 (inc x)))
                          concat-acc (concat acc map-coord)]
                      (recur concat-acc (count concat-acc) (inc x)))
                    )
                  )))
  )

;.;. The reward of a thing well done is to have done it. -- Emerson
( fact
  (coordinates 0) => []
  (coordinates 1) => [[0 0]]
  (coordinates 3) => [[0 0] [1 0] [1 1]]
  (coordinates 6) => [[0 0]
                      [1 0] [1 1]
                      [2 0] [2 1] [2 2]]
  (coordinates 10) => [[0 0]
                      [1 0] [1 1]
                      [2 0] [2 1] [2 2]
                      [3 0] [3 1] [3 2] [3 3]]
  )

(defn coord-from-pos "Determine the coordinate from the position"
  [n]
  (nth (coordinates (inc n)) n)
  )

(fact
  (coord-from-pos 0) => [0 0]
  (coord-from-pos 1) => [1 0]
  (coord-from-pos 2) => [1 1]
  (coord-from-pos 3) => [2 0]
  (coord-from-pos 9) => [3 3]
  )

; first: {:3 [{:7 [{:2 [8 5]} {:4 [5 9]}]} {:4 [{:4 [5 9]} {:6 [9 3]}]}]} -> heavy
;    3            [0 0]              0        p 1
;   7 4        [1 0] [1 1]          1 2       p 2
;  2 4 6     [2 0] [2 1] [2 2]     3 4 5      p 3
; 8 5 9 3  [3 0][3 1][3 2][3 3]   6 7 8 9     p 4

(defn construct-tree "Construct the graph from the sequence"
  ([seq] (construct-tree seq {} 0 0 (depth seq)))
  ([seq t x y depth-seq]
     (let [fst (first seq) count-s (count seq)]
       (println "fst" fst "count-s" count-s "depth-seq" depth-seq "(- count-s depth-seq)" (- count-s depth-seq))
       (if (= nil fst)
         t
         (if (pos? (- count-s depth-seq))
           (let [tree-updated {[x y] {:v fst :c [[(inc x) y] [(inc x) (inc y)]]}}]
             (conj t
                   (construct-tree (rest seq) tree-updated (inc x) y depth-seq)
                   (construct-tree (rest (rest seq)) tree-updated (inc x) (inc y) depth-seq)))
           (conj t {[x y] {:v fst :c []}})
           )
         ))))

;    3            [0 0]              0        depth 1
;   7 4        [1 0] [1 1]          1 2             2
;  2 4 6     [2 0] [2 1] [2 2]     3 4 5            3
; 8 5 9 3  [3 0][3 1][3 2][3 3]   6 7 8 9           4

(fact
;  (sort (construct-tree [3])) => {[0 0] {:v 3, :c []}}
;  (sort (construct-tree [3 7 4])) => {[0 0] {:v 3, :c [[1 0] [1 1]]}, [1 0] {:v 7, :c []}, [1 1] {:v 4, :c []}}
  (sort (construct-tree [3 7 4 2 10 6])) => '([[0 0] {:v 3, :c [[1 0] [1 1]]}]
                                     [[1 0] {:v 7, :c [[2 0] [2 1]]}] [[1 1] {:v 4, :c [[2 1] [2 2]]}]
                               [[2 0] {:v 2, :c []}]  [[2 1] {:v 10, :c []}]   [[2 2] {:v 6, :c []}])
;  (construct-tree [3 7 4 2 4 6 8 5 9 3]) => 0
  )
