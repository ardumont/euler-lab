(ns my-project-euler-lab.core-pb-prime
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]]))


                                        ; Prime numbers

(defn prime-numbers "Return the list of the n first prime numbers"
  [n]
  (loop [candidate 2 current n primes []]
    (if (zero? current)
      primes
      (if (every? #(not= 0 (rem candidate %)) primes)
        (recur (inc candidate) (dec current) (conj primes candidate))
        (recur (inc candidate) current primes)
        )))
  
  )
                                        ; list of the n primes number

(fact
  (prime-numbers 0) => '()
  (prime-numbers 1) => '(2)
  (prime-numbers 2) => '(2 3)
  (prime-numbers 3) => '(2 3 5)
  (prime-numbers 4) => '(2 3 5 7)
  (prime-numbers 100) => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541))


