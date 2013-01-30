(ns my-project-euler-lab.core18
  "Triangle max path"
  (:use [midje.sweet]))

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

(def seq-euler-18 [[75]
                   [95 64]
                   [17 47 82]
                   [18 35 87 10]
                   [20 4 82 47 65]
                   [19 1 23 75 3 34]
                   [88 2 77 73 7 63 67]
                   [99 65 4 28 6 16 70 92]
                   [41 41 26 56 83 40 80 70 33]
                   [41 48 72 33 47 32 37 16 94 29]
                   [53 71 44 65 25 43 91 52 97 51 14]
                   [70 11 33 28 77 73 17 78 39 68 17 57]
                   [91 71 52 38 17 14 91 43 58 50 27 29 48]
                   [63 66 4 68 89 53 67 30 73 16 69 87 40 31]
                   [4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]])

(defn mkc
  "Compute the children from a given node"
  [{:keys [c]} t]
  (let [[y x] c
        y+ (+ y 1)
        x+ (+ x 1)]
    (filter (comp not nil? :v) [{:c [y+ x] :v (get-in t [y+ x])}
                                {:c [y+ x+] :v (get-in t [y+ x+])}])))

(fact
  (mkc {:c [0 0]} [[1]
                   [2 4]
                   [5 1 4]
                   [2 3 4 5]]) => [{:c [1 0] :v 2} {:c [1 1] :v 4}]
  (mkc {:c [2 1]} [[1]
                   [2 4]
                   [5 1 4]
                   [2 3 4 5]]) => [{:c [3 1] :v 3} {:c [3 2] :v 4}]
  (mkc {:c [3 1]} [[1]
                   [2 4]
                   [5 1 4]
                   [2 3 4 5]]) => [])

(defn bfs
  [t]
  ((fn nx [q]
     (lazy-seq
      (when (seq q)
        (let [n (peek q)
              c (map #(update-in % [:v] + (:v n)) (mkc n t))]
          (cons n (nx (into (pop q) c)))))))
   (conj clojure.lang.PersistentQueue/EMPTY {:c [0 0] :v (get-in t [0 0])})))

(fact
  (take 3 (bfs [[1]
                [2 4]
                [5 1 4]
                [2 3 4 5]])) => '({:c [0 0], :v 1} {:c [1 0], :v 3} {:c [1 1], :v 5})
  (bfs [[1]
        [2 4]
        [5 1 4]
        [2 3 4 5]]) => '({:c [0 0], :v 1}
                         {:c [1 0], :v 3} {:c [1 1], :v 5}
                         {:c [2 0], :v 8} {:c [2 1], :v 4} {:c [2 1], :v 6} {:c [2 2], :v 9}
                         {:c [3 0], :v 10} {:c [3 1], :v 11} {:c [3 1], :v 7} {:c [3 2], :v 8} {:c [3 1], :v 9} {:c [3 2], :v 10} {:c [3 2], :v 13} {:c [3 3], :v 14}))

(defn max-path "Triangle maximal path"
  [t]
  (let [v (vec t)
        l (-> v count dec)]
    (->> v
         bfs
         (drop-while #(not= ((:c %) 0) l))
         (map :v)
         (apply max))))

(fact (max-path seq-euler-18) => 1074)
