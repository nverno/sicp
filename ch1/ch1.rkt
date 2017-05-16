#lang racket 

;;; Chapter 1, some exers
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; square function
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; -------------------------------------------------------------------
;;; 1.3
;; Return the sum of the squares of the largest two parameters
(define (sss x1 x2 x3)
  (let ((sorted (sort (list x1 x2 x3) >)))
    (+ (* (car sorted) (car sorted))
       (* (car (cdr sorted)) (car (cdr sorted))))))

(define (sss2 x1 x2 x3)
  (if (> x1 x2)
      (if (> x3 x2)
          (sum-of-squares x1 x3)
          (sum-of-squares x1 x2))
      (if (> x3 x1)
          (sum-of-squares x2 x3)
          (sum-of-squares x1 x2))))

(sss2 1 2 3)
(sss 1 2 3)  ; => 13
(sss2 3 1 2)
(sss 3 1 2)  ; => 13

;; Compound operator => if b is negative -, else +
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -1)

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (abs2 x)
  (if (< x 0) (- x) x))


(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Evaluating (test 0 (p))
;; * applicative order loop endlessly evaluating (p)
;; * normal-order
;; (1) substitutes for test:
;;   (if (= 0 0) 0 (p)) => 0

;; Starting from 1.1.8 in sicp

;; alternative square
(define (square2 x)
  (exp (double (log x))))
(define (double x) (+ x x))
(define (square x) (* x x))

;; Linear recursion and iteration
;; linearly recursive factorial
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; linear iterative factorial (maintain counter)
(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

;;; Exercise 1.10
;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; -------------------------------------------------------------------
;;; Exercise 1.11
;; f is defined by:
;; - f(n) = n if n<3
;; - f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n>=3

;; recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

;; iterative process
(define (f2 n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (- count 1))))

;; -------------------------------------------------------------------
;;; Exercise 1.12
;; compute elements of pascal's triangle recursively
;; this is just the binomial coefficients \frac{n}{k}
;; n: row
;; k: column
(define (pascal n k)
  (n-choose-k n k))

(define (n-choose-k n k)
  (cond ((> k n) 0)
	((or (= k 0) (= n k)) 1)
	(else (+ (n-choose-k (- n 1) k)
		 (n-choose-k (- n 1) (- k 1))))))

;; -------------------------------------------------------------------
;;; Ex. 1.13
;; Fib(n) closest to phi^n / sqrt(5), where phi = (1+sqrt(5))/2
(define phi (/ (+ 1 (sqrt 5)) 2))
(truncate (/ (+ 1 (expt phi 5)) (sqrt 5)))
(define sigma (/ (- 1 (sqrt 5)) 2))
