(ns euler-lab.core22
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.string :only [join split]]))

;; Using names.txt a 46k text file containing over 5000 first names, begin by sorting it into alphabetical order.
;; Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 3+15+12+9+14=53, is the 938th name in the list.
;; So COLIN would obtain a score of 938*53=49714.

;; What is the total of all the name scores in the file?

(unfinished)

(defn lweight "Compute the alphabetical weight of a letter"
  [l]
  (cond
   (= "A" l) 1
   (= "B" l) 2
   (= "C" l) 3
   (= "D" l) 4
   (= "E" l) 5
   (= "F" l) 6
   (= "G" l) 7
   (= "H" l) 8
   (= "I" l) 9
   (= "J" l) 10
   (= "K" l) 11
   (= "L" l) 12
   (= "M" l) 13
   (= "N" l) 14
   (= "O" l) 15
   (= "P" l) 16
   (= "Q" l) 17
   (= "R" l) 18
   (= "S" l) 19
   (= "T" l) 20
   (= "U" l) 21
   (= "V" l) 22
   (= "W" l) 23
   (= "X" l) 24
   (= "Y" l) 25
   (= "Z" l) 26))

(tabular
 (fact (lweight ?l) => ?r)
 ?l ?r
 "A" 1
 "B" 2
 "C" 3
 "D" 4
 "E" 5
 "F" 6
 "G" 7
 "H" 8
 "I" 9
 "J" 10
 "K" 11
 "L" 12
 "M" 13
 "N" 14
 "O" 15
 "P" 16
 "Q" 17
 "R" 18
 "S" 19
 "T" 20
 "U" 21
 "V" 22
 "W" 23
 "X" 24
 "Y" 25
 "Z" 26)

(defn wweight "Compute the weight of the word in the alphabet"
  [word]
  (let [vec-letters (vec word)]
    (reduce + (map #(lweight (str %)) vec-letters))))

(fact "Compute the weight of the word in the alphabet"
  (wweight "CHLOE") => 43
  (wweight "COLIN") => 53)

(defn sum-all-number-from-sorted-list "Compute the sum of the computation asked on a sorted list"
  [m-words]
  (let [keys-words (keys m-words)]
    (reduce +
            (map (fn [word] (*
                            (m-words word)
                            (wweight word)))
                 keys-words))))

(fact
  (sum-all-number-from-sorted-list {"antoine" 1 "test" 2}) => 100
  (provided
    (keys {"antoine" 1 "test" 2}) => ["antoine" "test"]
    (wweight "antoine") => 50
    (wweight "test") => 25)
  (sum-all-number-from-sorted-list {"CHLOE" 1 "COLIN" 2}) => 149)

; to work on the given file, strip the " and the ,
;(sed 's/[,]/ /gi' names.txt | sed 's/"//gi') > names-wq.txt

(def v-ordered-words (sort (split (slurp "/home/tony/repo/perso/euler-lab/src/my_project_euler_lab/names-wq.txt") #" ")))

(defn vec-to-map "Construct a map from a vec"
  [v]
  (let [count-v (count v)]
    (loop [acc {} lst v curr 1]
      (if (> curr count-v)
        acc
        (recur (assoc acc (first lst) curr) (rest lst) (inc curr))))))

(fact
  (vec-to-map ["toto" "theo" "foo"]) => {"toto" 1 "theo" 2 "foo" 3})

;.;. Simplicity, carried to the extreme, becomes elegance. -- Jon Franklin
(fact (sum-all-number-from-sorted-list (vec-to-map v-ordered-words)) => 871198282)
