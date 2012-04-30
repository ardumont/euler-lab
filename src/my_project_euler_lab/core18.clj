(ns my-project-euler-lab.core18
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.pprint             :only [pprint]])
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]]))

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

;    3
;   7 4
;  2 10 6
; 8 5  9 3
(def map-euler-18-simple (construct-tree [3 7 4 2 10 6 8 5 9 3]))

; try to use the already existing method to do the same

(def map-euler-18-simple-2 (construct-tree [3 7 4 2 4 6 8 5 9 3]))

(defn wt "Find the routes with the highest sum."
  ([m] (if (zero? (count m)) 0 (wt m [0 0])))
  ([m cd]
     (let [{:keys [v c s]} (m cd)]
       (concat [s]
               (mapcat (fn [ncd] (wt (update-in m [ncd :s] (fn [s1] (+ s s1))) ncd)) c)))))

(fact
  (wt map-euler-18-simple-2) =>  '(3 10 12 20 17 14 19 23 7 11 16 20 13 22 16))

(defn walk-tree-max-1 "Compute the max sum of the tree"
  [m]
  (let [v-sums-max (wt m)]
    (reduce max v-sums-max)))

(fact
  (walk-tree-max-1 map-euler-18-simple-2) =>  23)

(fact
  (walk-tree-max-1 map-euler-18) =>  1074)
