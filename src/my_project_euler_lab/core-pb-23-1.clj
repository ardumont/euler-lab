(ns my-project-euler-lab.core-pb-23-1
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.set :only [difference union]] )
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.primes :only [all-divisors-bi]]))

;A perfect number is a number for which the sum of its proper divisors
;is exactly equal to the number. 

;For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
;
;A number n is called deficient if the sum of its proper divisors is less than n 
;and it is called abundant if this sum exceeds n.

;As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;smallest number that can be written as the sum of 2 abundant
;numbers is 24. By mathematical analysis, it can be shown that all
;integers greater than 28123 can be written as the sum of 2 abundant
;numbers.

;However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number 
;that cannot be expressed as the sum of 2 abundant numbers is less than this limit.

;Find the sum of all the positive integers which cannot be written as the sum of 2 abundant numbers.

(unfinished)

(println "--------- PB 23-1 - naive way ----------" (java.util.Date.))

(defn abundant? "Test if a number is abundant (sums of its divisors is strictly superior to itself)"
  [n]
  (let [sum-all-divisors (reduce + (all-divisors-bi n))]
    (< n sum-all-divisors)))

(fact
  (abundant? 10) => false
  (abundant? 20) => true
  (abundant? 40) => true
  (abundant? 12) => true
  (abundant? 24) => true
  (abundant? 48) => true
  (abundant? 96) => true
  (abundant? 60) => true
  (abundant? 120) => true)

(defn sum-2-abundants? "Can the number be the sum of 2 abundants numbers?"
  [n]
  (let [half-n (ceil (/ n 2))
        integers-s (range 1 (inc half-n))]
    (some #(and (abundant? %) (abundant? (- n %))) integers-s)))

(fact
  (sum-2-abundants? 60) => true
  (sum-2-abundants? 120) => true
  (sum-2-abundants? 240) => true
  (sum-2-abundants? 28123) => true
  (sum-2-abundants? 12) => falsey)

(defn sum-2-abundants-numbers "Compute the set of the numbers that can be written as the sum of 2 abundant numbers."
  [n]
  (filter #(sum-2-abundants? %)
              (range 1 (inc n))))

;.;. Without work, all life goes rotten. -- Camus
(fact
  (sum-2-abundants-numbers 50)
  => (contains [32 36 38 40 42 44 48 50 24 30] :in-any-order)
  (sum-2-abundants-numbers 100)
  => (contains[32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94] :in-any-order))

(println "--------- PB 23-1 - algo with map ----------" (java.util.Date.))

(defn map-abundants "Make a map of abundant numbers."
  [n]
  (reduce conj {} (map #(if (abundant? %) {% %}) (range 1 (inc n)))))

(fact
  (map-abundants 24) => {24 24, 20 20, 18 18, 12 12})

(defn abundants-wm? "Another implementation to check if a number is abundant or not."
  [m-abundants n]
  (not= nil (m-abundants n)))

(fact
  (abundants-wm? (map-abundants 24) 10) => false
  (abundants-wm? (map-abundants 24) 12) => true)

(defn sum-2-abundants-wm? "Can the number be the sum of 2 abundants numbers?"
  [m-abundants n]
  (let [half-n (ceil (/ n 2))
        integers-s (range 1 (inc half-n))]
    (some #(and (abundant-wm? m-abundants %)
                (abundant-wm? m-abundants (- n %)))
          integers-s)))

(fact
  (sum-2-abundants-wm? (map-abundants 100) 60) => true
  (sum-2-abundants-wm? (map-abundants 120) 120) => true
  (sum-2-abundants-wm? (map-abundants 240) 240) => true
  (sum-2-abundants-wm? (map-abundants 20) 12) => falsey)

(defn sum-2-abundants-numbers-wm "Compute the set of the numbers that can be written as the sum of 2 abundant numbers."
  [m-abundant n]
  (filter #(sum-2-abundants-wm? m-abundant %)
          (range 1 (inc n))))

(fact
  (sum-2-abundants-numbers-wm (map-abundants 50)  50)
  => (contains [32 36 38 40 42 44 48 50 24 30] :in-any-order)
  (sum-2-abundants-numbers-wm (map-abundants 100) 100)
  => (contains [32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94] :in-any-order) )

(println "--------- PB 23-1 - New algo ----------" (java.util.Date.))

(defn double-til-limit "Double the number num until the limit limit is reached"
  [num limit]
  (loop [curr num acc #{}]
    (let [dcurr (* 2 curr)]
      (if (< limit dcurr)
        (conj acc curr)
        (if (<= 24 curr)
          (recur dcurr (conj acc curr))
          (recur dcurr acc)
          )))))

(fact (double-til-limit 12 21823) => #{3072 6144 12288 96 192 384 1536 768 48 24})

(defn init-sum-abundants "Function to init the set of the numbers that can be written as the sum of 2 abundant numbers - Use the map of abundant numbers."
  [m-abundants n]
  (reduce union #{} (map #(double-til-limit % n) (keys m-abundants))))

(fact
  (init-sum-abundants (map-abundants 100) 100) => #{96 66 36 100 70 40 72 42 78 48 80 84 54 24 56 88 90 60 30})

(defn sum-2-abundants-numbers-2 "Compute the set of the numbers that can be written as the sum of 2 abundant numbers."
  [m-abundant n]
  (if (< n 24)
    #{}
    (let [s-sum-2-abundants (init-sum-abundants m-abundant n)]
      (loop [acc s-sum-2-abundants s-integers (range 1 (inc n))]
        (let [fst (first s-integers)]
          (if (= nil fst)
            acc
            (if (or (acc fst) (not (sum-2-abundants-wm? m-abundant fst)))
              (recur acc (rest s-integers))
;              (recur (conj acc fst) (rest s-integers))
              (recur (union acc (double-til-limit fst n)) (rest s-integers))
              )))))))

;(def m-abundant (map-abundants 28123))

(fact "sum-2-abundants-numbers-2 - new way"
  (sum-2-abundants-numbers-2 (map-abundants 50) 50) => (just #{32 36 38 40 42 44 48 50 24 30}, :in-any-order)

  (sum-2-abundants-numbers-2 (map-abundants 100) 100) => (just #{24 30 32 36 38 40 42 44 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100} :in-any-order)

  (sum-2-abundants-numbers-2 (map-abundants 500) 500) => (just #{24 30 32 36 38 40 42 44 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198 200 202 204 206 208 210 212 214 216 218 220 222 224 226 228 230 232 234 236 238 240 242 244 246 248 250 252 254 256 258 260 262 264 266 268 270 272 274 276 278 280 282 284 286 288 290 292 294 296 298 300 302 304 306 308 310 312 314 316 318 320 322 324 326 328 330 332 334 336 338 340 342 344 346 348 350 352 354 356 358 360 362 364 366 368 370 372 374 376 378 380 382 384 386 388 390 392 394 396 398 400 402 404 406 408 410 412 414 416 418 420 422 424 426 428 430 432 434 436 438 440 442 444 446 448 450 452 454 456 458 460 462 464 466 468 470 472 474 476 478 480 482 484 486 488 490 492 494 496 498 500} :in-any-order))

(defn sum-all-positive-integer-euler-23 "Compute the sum of all positive integer which cannot be written as the sum of 2 abundants numbers in the interval 12 - n"
  [n]
  (let [sum-integers (reduce + (range 1 28124))
        m-abundants (map-abundants n)
        sum-abundants (reduce + (sum-2-abundants-numbers-2 m-abundants n))]
    (- sum-integers sum-abundants)))

(println "--------- END OF PB 23-1 ----------" (java.util.Date.))

