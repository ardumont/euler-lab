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

(tabular "Violent tests to see if the bisextile method is correct"
         (fact (leap-year? ?y) => ?expected)
         ?y    ?expected
         1804   truthy
         1808   truthy
         1812   truthy
         1816   truthy
         1820   truthy
         1824   truthy
         1828   truthy
         1832   truthy
         1836   truthy
         1840   truthy
         1844   truthy
         1848   truthy
         1852   truthy
         1856   truthy
         1860   truthy
         1864   truthy
         1868   truthy
         1872   truthy
         1876   truthy
         1880   truthy
         1884   truthy
         1888   truthy
         1892   truthy
         1896   truthy
         1904   truthy
         1908   truthy
         1912   truthy
         1916   truthy
         1920   truthy
         1924   truthy
         1928   truthy
         1932   truthy
         1936   truthy
         1940   truthy
         1944   truthy
         1948   truthy
         1952   truthy
         1956   truthy
         1960   truthy
         1964   truthy
         1968   truthy
         1972   truthy
         1976   truthy
         1980   truthy
         1984   truthy
         1988   truthy
         1992   truthy
         1996   truthy
         2000   truthy
         2004   truthy
         2008   truthy
         2012   truthy
         2016   truthy
         2020   truthy
         2024   truthy
         2028   truthy
         2032   truthy
         2036   truthy
         2040   truthy
         2044   truthy
         2048   truthy
         2052   truthy
         2056   truthy
         2060   truthy
         2064   truthy
         2068   truthy
         2072   truthy
         2076   truthy
         2080   truthy
         2084   truthy
         2088   truthy
         2092   truthy
         2096   truthy
         2104   truthy
         2108   truthy
         2112   truthy
         2116   truthy
         2120   truthy
         2124   truthy
         2128   truthy
         2132   truthy
         2136   truthy
         2140   truthy
         2144   truthy
         2148   truthy
         2152   truthy
         2156   truthy
         2160   truthy
         2164   truthy
         2168   truthy
         2172   truthy
         2176   truthy
         2180   truthy
         2184   truthy
         2188   truthy
         2192   truthy
         2196   truthy
         2204   truthy
         2208   truthy
         2212   truthy
         2216   truthy
         2220   truthy
         2224   truthy
         2228   truthy
         2232   truthy
         2236   truthy
         2240   truthy
         2244   truthy
         2248   truthy
         2252   truthy
         2256   truthy
         2260   truthy
         2264   truthy
         2268   truthy
         2272   truthy
         2276   truthy
         2280   truthy
         2284   truthy
         2288   truthy
         2292   truthy
         2296   truthy
         2304   truthy
         2308   truthy
         2312   truthy
         2316   truthy
         2320   truthy
         2324   truthy
         2328   truthy
         2332   truthy
         2336   truthy
         2340   truthy
         2344   truthy
         2348   truthy
         2352   truthy
         2356   truthy
         2360   truthy
         2364   truthy
         2368   truthy
         2372   truthy
         2376   truthy
         2380   truthy
         2384   truthy
         2388   truthy
         2392   truthy
         2396   truthy
         2400   truthy)
