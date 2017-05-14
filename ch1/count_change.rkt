#lang racket
;; Change counting problem
;; How many different ways to make change for a certain sum given
;; various change denominations

;; Simple tree-recursive solution: branch #denominations each time
;; ie, number of ways to make change w/o first coin +
;;     number of ways to make change for amount - (first coin value) using all n coins
;; coins: 1,5,10,25,50
;; (define (count-change amount)
;;   (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))
(count-change 100)  ; => 292

;; Better solution: use lookup table to store already computed values (memoize)
;; DP: counting all solutions, including permutations of coins as different
;; ways of making the count

;; (define (cc2 amount denominations)
;;   (let ((table ()))))
