(ns euler-lab.core17
  "Problem 17 - http://projecteuler.net/problem=17
If the numbers 1 to 5 are written out in words: one, two, three,
four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
If all the numbers from 1 to 1000 (one thousand) inclusive were written
 out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
and forty-two) contains 23 letters and 115 (one hundred and fifteen)
contains 20 letters. The use of and when writing out numbers is in
compliance with British usage."
  (:require [midje.sweet :as m]
            [euler-lab.utils :as utils]))

(def range-one-to-ten
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def range-twenty-to-ninety
  ["twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def word ^{:doc "Transco map number to word number"}
  {1 (zipmap (range 1 10)                 range-one-to-ten)
   2 (merge
      (zipmap (map str (range 10 20))     ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
      (zipmap (map str (range 20 100 10)) range-twenty-to-ninety)
      (zipmap (range 2 10)                range-twenty-to-ninety))
   3 (zipmap (range 1 10)                 range-one-to-ten)})

(defn substitute-word "Substitute the number by its letter version (only 3 levels, unity, dizaine and hundred)"
  [num range]
  (if (<= 3 range)
    (get-in word [range num] "one")
    (get-in word [range num])))

(m/tabular
 (m/fact (substitute-word ?in ?range) => ?out)
 ?in ?range ?out
 1 1 "one"
 2 1 "two"
 3 1 "three"
 4 1 "four"
 5 1 "five"
 6 1 "six"
 7 1 "seven"
 8 1 "eight"
 9 1 "nine"
 "10" 2 "ten"
 "11" 2 "eleven"
 "12" 2 "twelve"
 "13" 2 "thirteen"
 "14" 2 "fourteen"
 "15" 2 "fifteen"
 "16" 2 "sixteen"
 "17" 2 "seventeen"
 "18" 2 "eighteen"
 "19" 2 "nineteen"
 "20" 2 "twenty"
 "30" 2 "thirty"
 "40" 2 "forty"
 "50" 2 "fifty"
 "60" 2 "sixty"
 "70" 2 "seventy"
 "80" 2 "eighty"
 "90" 2 "ninety"
 2 2 "twenty"
 3 2 "thirty"
 4 2 "forty"
 5 2 "fifty"
 6 2 "sixty"
 7 2 "seventy"
 8 2 "eighty"
 9 2 "ninety"
 1 3 "one"
 2 3 "two"
 3 3 "three"
 4 3 "four"
 5 3 "five"
 6 3 "six"
 7 3 "seven"
 8 3 "eight"
 9 3 "nine"
 1 4 "one")

(defn to-eng "Translate a number into plain old english"
  [vec-number]
  (loop [vec-num vec-number acc []]
    (let [count-vec (count vec-num)]
      (if (zero? count-vec)
        acc
        (let [fst (first vec-num)]
          (if (= 1 count-vec)
            (conj acc (substitute-word fst 1))
            (if (= 2 count-vec)
              (if (or (= fst 1) (zero? (second vec-num)))
                (conj acc (substitute-word (str fst (second vec-num)) 2))
                (recur (rest vec-num) (conj acc (substitute-word fst 2))))
              (if (= 3 count-vec)
                (if (and (zero? (second vec-num)) (zero? (nth vec-num 2)))
                  [(substitute-word fst 3) "hundred"]
                  (recur (rest vec-num) (conj acc (substitute-word fst 3) "hundred" "and")))
                (if (= 4 count-vec) ["one" "thousand"])))))))))

(m/fact
  (to-eng [3])       => ["three"]
  (to-eng [1 0])     => ["ten"]
  (to-eng [1 1])     => ["eleven"]
  (to-eng [1 2])     => ["twelve"]
  (to-eng [1 3])     => ["thirteen"]
  (to-eng [1 4])     => ["fourteen"]
  (to-eng [1 5])     => ["fifteen"]
  (to-eng [1 6])     => ["sixteen"]
  (to-eng [1 7])     => ["seventeen"]
  (to-eng [1 8])     => ["eighteen"]
  (to-eng [1 9])     => ["nineteen"]
  (to-eng [2 0])     => ["twenty"]
  (to-eng [9 0])     => ["ninety"]
  (to-eng [9 9])     => ["ninety" "nine"]
  (to-eng [1 0 0])   => ["one" "hundred"]
  (to-eng [1 0 1])   => ["one" "hundred" "and" nil "one"]
  (to-eng [3 0 0])   => ["three" "hundred"]
  (to-eng [6 0 0])   => ["six" "hundred"]
  (to-eng [1 2 3])   => ["one" "hundred" "and" "twenty" "three"]
  (to-eng [2 4 9])   => ["two" "hundred" "and" "forty" "nine"]
  (to-eng [1 0 0 0]) => ["one" "thousand"])

(defn to-eng-count "Translate a number into plain old english"
  [vec-number]
  (let [to-eng-vec (to-eng vec-number)]
    (reduce + (map count to-eng-vec))))

(m/fact
  (to-eng-count [3])       => 5
  (to-eng-count [8])       => 5
  (to-eng-count [9])       => 4
  (to-eng-count [1 0])     => 3
  (to-eng-count [1 1])     => 6
  (to-eng-count [1 2])     => 6
  (to-eng-count [1 3])     => 8
  (to-eng-count [1 4])     => 8
  (to-eng-count [1 5])     => 7
  (to-eng-count [1 6])     => 7
  (to-eng-count [1 7])     => 9
  (to-eng-count [1 8])     => 8
  (to-eng-count [1 9])     => 8
  (to-eng-count [2 0])     => 6
  (to-eng-count [8 1])     => 9
  (to-eng-count [9 0])     => 6
  (to-eng-count [9 9])     => 10
  (to-eng-count [1 0 1])   => 16
  (to-eng [1 2 3])         => ["one" "hundred" "and" "twenty" "three"]
  (to-eng-count [1 2 3])   => 24
  (to-eng [2 4 9])         => ["two" "hundred" "and" "forty" "nine"]
  (to-eng-count [2 4 9])   => 22
  (to-eng [3 4 2])         => ["three" "hundred" "and" "forty" "two"]
  (to-eng-count [3 4 2])   => 23
  (to-eng [1 1 5])         => ["one" "hundred" "and" "fifteen"]
  (to-eng-count [1 1 5])   => 20
  (to-eng-count [1 0 0 0]) => 11)

(defn substitute-words "Count all numbers in letters"
  [start end]
  (reduce + (map to-eng-count (map utils/num-digits-into-vec (range start (inc end))))))

(m/fact
  (substitute-words 1 1000) => 21124)
