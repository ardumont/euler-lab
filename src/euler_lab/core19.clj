(ns euler-lab.core19
  "Problem 19 - http://projecteuler.net/problem=19
You are given the following information, but you may prefer to do some research for yourself.

   * 1 Jan 1900 was a Monday.
   * Thirty days has September,
     April, June and November.
     All the rest have thirty-one,
     Saving February alone,
     Which has twenty-eight, rain or shine.
     And on leap years, twenty-nine.
   * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?"
  (:require [clj-time.core :as time]
            [midje.sweet   :as m]))

(defn leap-year? "Is the year bisextile?"
  [y]
  (or (zero? (rem y 400)) (and (zero? (rem y 4)) (not= 0 (rem y 100)))))

(m/fact "Determine if a year is a leap or not"
  (leap-year? 400)  => m/truthy
  (leap-year? 2008) => m/truthy
  (leap-year? 2012) => m/truthy
  (leap-year? 200)  => m/falsey)

(defn ndays "Number of days per month and leap year"
  [m l]
  (cond
   (= m "jan") 31
   (= m "feb") (if (true? l) 29 28)
   (= m "mar") 31
   (= m "apr") 30
   (= m "may") 31
   (= m "jun") 30
   (= m "jul") 31
   (= m "aug") 31
   (= m "sep") 30
   (= m "oct") 31
   (= m "nov") 30
   (= m "dec") 31))

(m/tabular "Number of days per month and leap year"
 (m/fact
   (ndays ?month ?leap) => ?days)
 ?month ?leap ?days
 "jan" .blah. 31
 "feb"  true  29
 "feb"  false 28
 "mar" .blah. 31
 "apr" .blah. 30
 "may" .blah. 31
 "jun" .blah. 30
 "jul" .blah. 31
 "aug" .blah. 31
 "sep" .blah. 30
 "oct" .blah. 31
 "nov" .blah. 30
 "dec" .blah. 31)

(defn first-sunday? "Is the first day of the month of the year a sunday?"
  [year month]
  (if (= 7 (time/day-of-week (time/date-time year month))) 1 0))

(m/fact "Check if the first day of the month is a sunday or not"
  (first-sunday? 1900 1)  => 0
  (first-sunday? 2011 5)  => 1)

(defn how-many-years-in-interval "how many years are there in the interval"
  [date-start date-end]
  (- date-end date-start 1))

(m/fact "Test the computing of years in interval."
  (how-many-years-in-interval 1900 1902) => 1
  (how-many-years-in-interval 1900 2000) => 99)

(defn how-many-first-sunday-in-year "How many first sunday is there in a year"
  [y]
  (let [vmonth (range 1 13)]
    (reduce + (map (fn [month] (first-sunday? y month)) vmonth))))

(defn how-many-first-sundays "How many sundays a there between 2 points in time?"
  [date-start date-end]
  (let [nb-years (how-many-years-in-interval date-start date-end)
        interval-years (take nb-years (iterate inc (inc date-start)))]
    (reduce +
            (map (fn [year] (how-many-first-sunday-in-year year)) interval-years))))

(m/fact "Facts about the computations of first sundays on a period"
  (how-many-first-sundays 1900 2001) => 171)
