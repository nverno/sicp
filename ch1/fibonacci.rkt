#lang racket 
;; Computing fibonacci numbers: 0,1,1,2,3,5,8,13,21,...
;; Tree recursion: very inefficient, recursion branching each call
;; exponential time (grows as fast as fib(n) itself),
;; but small space required (~ depth of tree)
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 2))
		 (fib (- n 1))))))

;; linearly iterative version: much faster
;; initialize Fib(1) = 1, Fib(0) = 0
;; a <- a + b
;; b <- a
;; After n transformations, a and b will be equal to Fib(n+1) and Fib(n)
(define (fib1 n)
  (fib-iter1 1 0 n))

(define (fib-iter1 a b count)
  (if (= count 0)
      b
      (fib-iter1 (+ a b) a (- count 1))))

;; -------------------------------------------------------------------
;;; Exercise 1.19
;; compute fibonacci numbers in log steps
;; write T_pq as:
;; [ q+p q ]
;; [  q  p ]
;; so, [a b] %*% T_pq => [a(q+p)+bq, aq+bp]
;; Then, compute T_p'q' = (T_pq)^2
;; T_p'q' = [ (q+p)^2+q^2 , q(q+p)+qp ]
;;          [ q(q+p)+qp   , q^2+p^2   ]
;; thus, p' = q^2+p^2, q' = q^2 + 2pq
(define (fib2 n)
  (fib-iter2 1 0 0 1 n))

(define (fib-iter2 a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter2 a
		    b
		    (+ (* p p) (* q q))
		    (+ (* q q) (* 2 p q))
		    (/ count 2)))
	(else (fib-iter2 (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))
