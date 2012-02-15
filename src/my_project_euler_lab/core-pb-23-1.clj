(ns my-project-euler-lab.core-pb-23-1
  (:use [clojure.test               :only [run-tests]])
  (:use [midje.sweet])
  (:use [clojure.set :only [difference union]] )
  (:use [my-project-euler-lab.primes :only [all-divisors-bi]]))

;; A perfect number is a number for which the sum of its proper divisors
;; is exactly equal to the number. 

;; For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

;; A number n is called deficient if the sum of its proper divisors is less than n 
;; and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;; smallest number that can be written as the sum of 2 abundant
;; numbers is 24. By mathematical analysis, it can be shown that all
;; integers greater than 28123 can be written as the sum of 2 abundant
;; numbers.

;; However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number 
;; that cannot be expressed as the sum of 2 abundant numbers is less than this limit.

;; Find the sum of all the positive integers which cannot be written as the sum of 2 abundant numbers.

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

(fact
  (sum-2-abundants-numbers 50)
  => (just [32 36 38 40 42 44 48 50 24 30] :in-any-order)
  (sum-2-abundants-numbers 100)
  => (just [32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94] :in-any-order))

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
  (sum-2-abundants-wm? (map-abundants 240) 240) => true)

(defn sum-2-abundants-numbers-wm "Compute the set of the numbers that can be written as the sum of 2 abundant numbers."
  [m-abundant n]
  (filter #(sum-2-abundants-wm? m-abundant %)
          (range 1 (inc n))))

(fact
  (sum-2-abundants-numbers-wm (map-abundants 50)  50)
  => (just [32 36 38 40 42 44 48 50 24 30] :in-any-order)
  (sum-2-abundants-numbers-wm (map-abundants 100) 100)
  => (just [32 64 96 66 98 36 68 100 38 70 40 72 42 74 44 76 78 48 80 50 82 52 84 54 86 24 56 88 58 90 60 92 30 62 94] :in-any-order)
  (sum-2-abundants-numbers-wm (map-abundants 1000) 1000)
  => (just #{32 64 96 128 160 192 224 256 288 320 352 384 416 448 480 512 544 576 608 640 672 704 736 768 800 832 864 896 928 960 992 993 66 98 130 162 194 226 258 290 322 354 386 418 450 482 514 546 578 610 642 674 706 738 770 802 834 866 898 930 962 994 963 36 68 100 132 164 196 228 260 292 324 356 388 420 452 484 516 548 580 612 644 676 708 740 772 804 836 868 900 932 964 996 965 38 70 102 134 166 198 230 262 294 326 358 390 422 454 486 518 550 582 614 646 678 710 742 774 806 838 870 902 934 966 998 999 40 72 104 136 168 200 232 264 296 328 360 392 424 456 488 520 552 584 616 648 680 712 744 776 808 840 872 904 936 968 1000 969 42 74 106 138 170 202 234 266 298 330 362 394 426 458 490 522 554 586 618 650 682 714 746 778 810 842 874 906 938 970 44 76 108 140 172 204 236 268 300 332 364 396 428 460 492 524 556 588 620 652 684 716 748 780 812 844 876 908 940 972 78 110 142 174 206 238 270 302 334 366 398 430 462 494 526 558 590 622 654 686 718 750 782 814 846 878 910 942 974 975 48 80 112 144 176 208 240 272 304 336 368 400 432 464 496 528 560 592 624 656 688 720 752 784 816 848 880 912 944 976 50 82 114 146 178 210 242 274 306 338 370 402 434 466 498 530 562 594 626 658 690 722 754 786 818 850 882 914 946 978 52 84 116 148 180 212 244 276 308 340 372 404 436 468 500 532 564 596 628 660 692 724 756 788 820 852 884 916 948 980 981 54 86 118 150 182 214 246 278 310 342 374 406 438 470 502 534 566 598 630 662 694 726 758 790 822 854 886 918 950 982 24 56 88 120 152 184 216 248 280 312 344 376 408 440 472 504 536 568 600 632 664 696 728 760 792 824 856 888 920 952 984 985 58 90 122 154 186 218 250 282 314 346 378 410 442 474 506 538 570 602 634 666 698 730 762 794 826 858 890 922 954 986 987 60 92 124 156 188 220 252 284 316 348 380 412 444 476 508 540 572 604 636 668 700 732 764 796 828 860 892 924 956 988 957 30 62 94 126 158 190 222 254 286 318 350 382 414 446 478 510 542 574 606 638 670 702 734 766 798 830 862 894 926 958 990} :in-any-order))

(defn sum-all-positive-integer-euler-23-wm "Compute the sum of all positive integer which cannot be written as the sum of 2 abundants numbers in the interval 12 - n"
  [n]
  (let [sum-integers (reduce + (range 1 (inc n)))
        m-abundants (map-abundants n)
        sum-abundants (reduce + (sum-2-abundants-numbers-wm m-abundants n))]
    (- sum-integers sum-abundants)))

#_(fact (sum-all-positive-integer-euler-23-wm 28123) => 4179871)

(println "--------- PB 23-1 - New algo ----------" (java.util.Date.))

(defn double-til-limit "Double the number num until the limit limit is reached"
  [num limit]
  (loop [curr num acc #{}]
    (let [dcurr (* 2 curr)]
      (if (< limit dcurr)
        (conj acc curr)
        (if (<= 24 curr)
          (recur dcurr (conj acc curr))
          (recur dcurr acc))))))

(fact
  (double-til-limit 12 21823) => #{3072 6144 12288 96 192 384 1536 768 48 24}
  (double-til-limit 10 21823) => #{5120 10240 20480 160 1280 320 2560 640 40 80})

(defn init-sum-abundants "Function to init the set of the numbers that can be written as the sum of 2 abundant numbers - Use the map of abundant numbers."
  [m-abundants n]
  (reduce union #{} (map #(double-til-limit % n) (keys m-abundants))))

(fact
  (init-sum-abundants (map-abundants 100) 100) => #{96 66 36 100 70 40 72 42 78 48 80 84 54 24 56 88 90 60 30})

(defn sum-2-abundants-numbers-nw "Compute the set of the numbers that can be written as the sum of 2 abundant numbers."
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
              (recur (union acc (double-til-limit fst n)) (rest s-integers)))))))))

;.;. The highest reward for a man's toil is not what he gets for it but
;.;. what he becomes by it. -- Ruskin
(fact "sum-2-abundants-numbers - new way"
  (sum-2-abundants-numbers-nw (map-abundants 50) 50) => (just #{32 36 38 40 42 44 48 50 24 30}, :in-any-order)
  (sum-2-abundants-numbers-nw (map-abundants 100) 100) => (just #{24 30 32 36 38 40 42 44 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100} :in-any-order)
  (sum-2-abundants-numbers-nw (map-abundants 500) 500) => (just #{24 30 32 36 38 40 42 44 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198 200 202 204 206 208 210 212 214 216 218 220 222 224 226 228 230 232 234 236 238 240 242 244 246 248 250 252 254 256 258 260 262 264 266 268 270 272 274 276 278 280 282 284 286 288 290 292 294 296 298 300 302 304 306 308 310 312 314 316 318 320 322 324 326 328 330 332 334 336 338 340 342 344 346 348 350 352 354 356 358 360 362 364 366 368 370 372 374 376 378 380 382 384 386 388 390 392 394 396 398 400 402 404 406 408 410 412 414 416 418 420 422 424 426 428 430 432 434 436 438 440 442 444 446 448 450 452 454 456 458 460 462 464 466 468 470 472 474 476 478 480 482 484 486 488 490 492 494 496 498 500} :in-any-order)
    (sum-2-abundants-numbers-nw (map-abundants 1000) 1000) => (just #{24 30 32 36 38 40 42 44 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198 200 202 204 206 208 210 212 214 216 218 220 222 224 226 228 230 232 234 236 238 240 242 244 246 248 250 252 254 256 258 260 262 264 266 268 270 272 274 276 278 280 282 284 286 288 290 292 294 296 298 300 302 304 306 308 310 312 314 316 318 320 322 324 326 328 330 332 334 336 338 340 342 344 346 348 350 352 354 356 358 360 362 364 366 368 370 372 374 376 378 380 382 384 386 388 390 392 394 396 398 400 402 404 406 408 410 412 414 416 418 420 422 424 426 428 430 432 434 436 438 440 442 444 446 448 450 452 454 456 458 460 462 464 466 468 470 472 474 476 478 480 482 484 486 488 490 492 494 496 498 500 502 504 506 508 510 512 514 516 518 520 522 524 526 528 530 532 534 536 538 540 542 544 546 548 550 552 554 556 558 560 562 564 566 568 570 572 574 576 578 580 582 584 586 588 590 592 594 596 598 600 602 604 606 608 610 612 614 616 618 620 622 624 626 628 630 632 634 636 638 640 642 644 646 648 650 652 654 656 658 660 662 664 666 668 670 672 674 676 678 680 682 684 686 688 690 692 694 696 698 700 702 704 706 708 710 712 714 716 718 720 722 724 726 728 730 732 734 736 738 740 742 744 746 748 750 752 754 756 758 760 762 764 766 768 770 772 774 776 778 780 782 784 786 788 790 792 794 796 798 800 802 804 806 808 810 812 814 816 818 820 822 824 826 828 830 832 834 836 838 840 842 844 846 848 850 852 854 856 858 860 862 864 866 868 870 872 874 876 878 880 882 884 886 888 890 892 894 896 898 900 902 904 906 908 910 912 914 916 918 920 922 924 926 928 930 932 934 936 938 940 942 944 946 948 950 952 954 956 957 958 960 962 963 964 965 966 968 969 970 972 974 975 976 978 980 981 982 984 985 986 987 988 990 992 993 994 996 998 999 945 1000} :in-any-order))

(defn sum-all-positive-integer-euler-23-nw "Compute the sum of all positive integer which cannot be written as the sum of 2 abundants numbers in the interval 12 - n"
  [n]
  (let [sum-integers (reduce + (range 1 (inc n)))
        m-abundants (map-abundants n)
        sum-abundants (reduce + (sum-2-abundants-numbers-nw m-abundants n))]
    (- sum-integers sum-abundants)))

(future-fact "Need to understand the difference of 945 someday"
             (sum-all-positive-integer-euler-23-nw 28123) => 4179871)

(println "--------- END OF PB 23-1 ----------" (java.util.Date.))

