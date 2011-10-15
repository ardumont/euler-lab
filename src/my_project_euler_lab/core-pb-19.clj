(ns my-project-euler-lab.core-pb-19
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.contrib.repl-utils :only [show]])
  (:use [clojure.pprint             :only [pprint]])
  (:use [clojure.walk               :only [macroexpand-all]])
  (:use clojure.contrib.math)
  (:use [my-project-euler-lab.utils :only [num-digits-into-vec]])
  (:require [clojure.walk :as walk])
  )

;You are given the following information, but you may prefer to do some research for yourself.
;
;    * 1 Jan 1900 was a Monday.
;    * Thirty days has September,
;      April, June and November.
;      All the rest have thirty-one,
;      Saving February alone,
;      Which has twenty-eight, rain or shine.
;      And on leap years, twenty-nine.
;    * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
;
;How many Sundays fell on the first of the month during the twentieth
;century (1 Jan 1901 to 31 Dec 2000)?

(unfinished )

(defn leap-year? "Is the year bisextile?"
  [y]
  (or (zero? (rem y 400)) (and (zero? (rem y 4)) (not= 0 (rem y 100)))))

(fact "Determine if a year is a leap or not"
  (leap-year? 400) => truthy
  (leap-year? 2008) => truthy
  (leap-year? 2012) => truthy
  (leap-year? 200) => falsey
  )

(defn days "Number of days per month and leap year"
  [m l]
  (cond
   (= m "january") 31
   (= m "february") (if (true? l) 29 28)
   (= m "march") 31
   (= m "april") 30
   (= m "may") 31
   (= m "june") 30
   (= m "july") 31
   (= m "august") 31
   (= m "september") 30
   (= m "october") 31
   (= m "november") 30
   (= m "december") 31))

;.;. It takes time to succeed because success is merely the natural reward
;.;. of taking time to do anything well. -- Ross
(tabular "Number of days per month and leap year"
 (fact
   (days ?month ?leap) => ?days)
 ?month     ?leap   ?days
 "january"  .blah.  31
 "february"  true   29
 "february"  false  28
 "march"     .blah. 31
 "april"     .blah. 30
 "may"       .blah. 31
 "june"      .blah. 30
 "july"      .blah. 31
 "august"    .blah. 31
 "september" .blah. 30
 "october"   .blah. 31
 "november"  .blah. 30
 "december"  .blah. 31
 
 )
