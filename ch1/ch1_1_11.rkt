;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.11
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.12
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


;; Fib(n) closest to phi^n / sqrt(5), where phi = (1+sqrt(5))/2
(define phi (/ (+ 1 (sqrt 5)) 2))
(truncate (/ (+ 1 (expt phi 5)) (sqrt 5)))
(define sigma (/ (- 1 (sqrt 5)) 2))


