(ns my-project-euler-lab.core18
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

;;   (:require [clojure.walk :as walk])

;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
;;    3
;;   7 4
;;  2 4 6
;; 8 5 9 3

;; That is, 3 + 7 + 4 + 9 = 23.

;; Find the maximum total from top to bottom of the triangle below:

                 ;; 75
                ;; 95 64
              ;; 17 47 82
             ;; 18 35 87 10
            ;; 20 04 82 47 65
           ;; 19 01 23 75 03 34
          ;; 88 02 77 73 07 63 67
         ;; 99 65 04 28 06 16 70 92
        ;; 41 41 26 56 83 40 80 70 33
       ;; 41 48 72 33 47 32 37 16 94 29
      ;; 53 71 44 65 25 43 91 52 97 51 14
     ;; 70 11 33 28 77 73 17 78 39 68 17 57
    ;; 91 71 52 38 17 14 91 43 58 50 27 29 48
  ;; 63 66 04 68 89 53 67 30 73 16 69 87 40 31
 ;; 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

;; NOTE: As there are only 16384 routes, it is possible to solve this
;; problem by trying every route. However, Problem 67, is the same
;; challenge with a triangle containing one-hundred rows
;; it cannot be solved by brute force, and requires a clever method! ;o)

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
          (recur (inc curr) (rest map-triangle)))))))

(fact
  (depth [1]) => 1
  (depth [10 3 3]) => 2
  ; not a triangle this one!
  (depth [10 3 3 1 2 3]) => 3
  (depth [10 3 3 1 2 3 4]) => nil)

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
                      (recur concat-acc (count concat-acc) (inc x))))))))

(fact
  (coordinates 0) => []
  (coordinates 1) => [[0 0]]
  (coordinates 3) => [[0 0] [1 0] [1 1]]
  (coordinates 6) => [[0 0]
                      [1 0] [1 1]
                      [2 0] [2 1] [2 2]]
  (coordinates 10) => [[0 0]
                      [1 0] [1 1]
                      [2 0] [2 1] [2 2]
                      [3 0] [3 1] [3 2] [3 3]])

(defn coord-from-pos "Determine the coordinate from the position"
  [n]
  (nth (coordinates (inc n)) n))

(fact
  (coord-from-pos 0) => [0 0]
  (coord-from-pos 1) => [1 0]
  (coord-from-pos 2) => [1 1]
  (coord-from-pos 3) => [2 0]
  (coord-from-pos 9) => [3 3])

; first: {:3 [{:7 [{:2 [8 5]} {:4 [5 9]}]} {:4 [{:4 [5 9]} {:6 [9 3]}]}]} -> heavy
;    3            [0 0]              0        p 1
;   7 4        [1 0] [1 1]          1 2       p 2
;  2 4 6     [2 0] [2 1] [2 2]     3 4 5      p 3
; 8 5 9 3  [3 0][3 1][3 2][3 3]   6 7 8 9     p 4

(defn construct-tree "Construct the graph from the sequence"
  [seq]
  (let [count-s (count seq)
        depth-s (depth seq)]
    (loop [my-seq seq
           map-tree {}
           curr 0]
      (if (= curr count-s)
        map-tree
        (let [coord (coord-from-pos curr)
              fst (first my-seq)]
          (if (< 0 (- (count my-seq) depth-s))
            (let [x (inc (coord 0))
                  y (coord 1)]
              (recur (rest my-seq)
                     (assoc map-tree coord {:v fst, :c [[x y] [x (inc y)]], :s fst})
                     (inc curr)))
            (recur (rest my-seq)
                   (assoc map-tree coord {:v fst, :c [], :s fst})
                   (inc curr))))))))

; Now i can integrate the sequence into a triangle
(def seq-euler-18 [75
                   95 64
                   17 47 82
                   18 35 87 10
                   20 4 82 47 65
                   19 1 23 75 3 34
                   88 2 77 73 7 63 67
                   99 65 4 28 6 16 70 92
                   41 41 26 56 83 40 80 70 33
                   41 48 72 33 47 32 37 16 94 29
                   53 71 44 65 25 43 91 52 97 51 14
                   70 11 33 28 77 73 17 78 39 68 17 57
                   91 71 52 38 17 14 91 43 58 50 27 29 48
                   63 66 4 68 89 53 67 30 73 16 69 87 40 31
                   4 62 98 27 23 9 70 98 73 93 38 53 60 4 23])

(def map-euler-18 (construct-tree seq-euler-18))

(fact
  (construct-tree [3]) => (contains {[0 0] {:v 3 :c [] :s 3}} :in-any-order)
  (construct-tree [3 7 4]) => (contains { [0 0] {:v 3, :c [[1 0] [1 1]] :s 3},
                                          [1 0] {:v 7, :c [] :s 7}, [1 1] {:v 4, :c [] :s 4}} :in-any-order)
  (construct-tree [3 7 4 2 10 6]) => (contains { [0 0] {:v 3, :c [[1 0] [1 1]], :s 3},
                                                 [1 0] {:v 7, :c [[2 0] [2 1]], :s 7}, [1 1] {:v 4, :c [[2 1] [2 2]], :s 4},
                                                 [2 0] {:v 2, :c [], :s 2},  [2 1] {:v 10, :c [], :s 10},   [2 2] {:v 6, :c [], :s 6}} :in-any-order)
  (construct-tree [3 7 4 2 10 6 8 5 9 3]) => (contains { [0 0] {:v 3, :c [[1 0] [1 1]], :s 3},
                                                         [1 0] {:v 7, :c [[2 0] [2 1]], :s 7}, [1 1] {:v 4, :c [[2 1] [2 2]], :s 4},
                                                         [2 0] {:v 2, :c [[3 0] [3 1]], :s 2},  [2 1] {:v 10, :c [[3 1] [3 2]], :s 10},   [2 2] {:v 6, :c [[3 2] [3 3]], :s 6}
                                                         [3 0] {:v 8, :c [], :s 8},  [3 1] {:v 5, :c [], :s 5},  [3 2] {:v 9, :c [], :s 9}, [3 3] {:v 3, :c [] , :s 3}} :in-any-order :gaps-ok))

(defn tval "Retrieve the value of the map t with the key coord"
  [tree-map coord]
  ((tree-map coord) :v))

(fact "test thats the maps is correctly built"
  (tval map-euler-18 [0 0]) => 75
  (tval map-euler-18 [1 0]) => 95
  (tval map-euler-18 [14 14]) => 23)

(defn tchildren "Given a map and coordinates, give the children for the node with coord"
  [tree-map coord]
  ((tree-map coord) :c))

(fact "Check that given a map and coordinates, i find the children of the node"
  (tchildren map-euler-18 [0 0]) => [[1 0] [1 1]]
  (tchildren map-euler-18 [1 0]) => [[2 0] [2 1]]
  (tchildren map-euler-18 [14 14]) => [])

(defn walk-tree "Walk through the map"
  ([m]
     (if (zero? (count m))
       []
       (walk-tree m [0 0])))
  ([m coord]
     (let [val ((m coord) :v)
           vchild (sort ((m coord) :c))]
       (concat [val] (mapcat (fn [coordinates] (walk-tree m coordinates)) vchild)))))

;    3
;   7 4
;  2 10 6
; 8 5  9 3
(def map-euler-18-simple (construct-tree [3 7 4 2 10 6 8 5 9 3]))

(fact
  (walk-tree map-euler-18-simple) => '(3 7 2 8 5 10 5 9 4 10 5 9 6 9 3))

; try to use the already existing method to do the same

#_(walk/postwalk-demo map-euler-18)
#_(walk/prewalk-demo map-euler-18)
#_(walk/walk println identity map-euler-18)

; #fail

(defn walk-tree-max-0 "Find the routes with the highest sum."
  ( [m]
      (if (zero? (count m))
        0
        (walk-tree-max-0 m [0 0])))
  ([m coord]
     (let [val ((m coord) :v)
           vchild ((m coord) :c)
           sum-max ((m coord) :s)]
;       (println coord "=> :v" val ":c" vchild ":s" sum-max)
       (if (zero? (count vchild))
         sum-max
         (let [coord-1 (first vchild)
               coord-2 (last vchild)
               val-1 ((m coord-1) :v)
               val-2 ((m coord-2) :v)
               chi-1 ((m coord-1) :c)
               chi-2 ((m coord-2) :c)]
           (if (< val-1 val-2)
             (let [upd-m (assoc m coord-2 {:v val-2 :c chi-2 :s (+ val-2 sum-max)} )]
                 (walk-tree-max-0 upd-m coord-2))
             (let [upd-m (assoc m coord-1 {:v val-1 :c chi-1 :s (+ val-1 sum-max)} )]
                 (walk-tree-max-0 upd-m coord-1))))))))

(def map-euler-18-simple-2 (construct-tree [3 7 4 2 4 6 8 5 9 3]))

(fact
  (walk-tree-max-0 {}) => 0
  (walk-tree-max-0 {[0 0] {:v 10 :c [] :s 10}}) => 10
  (walk-tree-max-0 map-euler-18-simple) => 29
  (walk-tree-max-0 map-euler-18-simple-2) => 23)

;# fail -> too naive -> i must trip through all the nodes of the tree

(defn walk-tree-euler-18 "Find the routes with the highest sum."
  ([m]
      (if (zero? (count m))
        0
        (walk-tree-euler-18 m [0 0])))
  ([m coord]
     (let [val ((m coord) :v)
           vchild ((m coord) :c)
           sum-max ((m coord) :s)]
       (concat [sum-max]
               (mapcat (fn [coordinates]
                         (let [val-1 ((m coordinates) :v)
                               chi-1 ((m coordinates) :c)]
                           (let [upd-m (assoc m coordinates {:v val-1 :c chi-1 :s (+ val-1 sum-max)} )]
                             (walk-tree-euler-18 upd-m coordinates))))
                       vchild)))))

(fact
  (walk-tree-euler-18 map-euler-18-simple-2) =>  '(3 10 12 20 17 14 19 23 7 11 16 20 13 22 16))

(defn walk-tree-max-1 "Compute the max sum of the tree"
  [m]
  (let [v-sums-max (walk-tree-euler-18 m)]
    (reduce max v-sums-max)))

;.;. Out of clutter find simplicity; from discord find harmony; in the
;.;.                               ; middle of difficulty lies
;.;.                               ; opportunity. -- Einstein
(fact
  (walk-tree-max-1 map-euler-18-simple-2) =>  23
  (walk-tree-max-1 map-euler-18) =>  1074)
