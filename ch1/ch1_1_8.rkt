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

;; Exercise 1.10
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


