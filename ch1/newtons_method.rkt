;; Newton's Method
;; Compute square roots by successive approximations
;; We guess 'y' for a value of 'x' and improve the approximation by
;; averaging 'y' with 'x/y'

;; Iterative process
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

;; Improving the guess (average with quotient of radicand and old guess
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; good-enough? is the tolerance, here 0.001
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; Helper functions
(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))

;; Test
(exact->inexact (sqrt-iter 1 2))
(exact->inexact (sqrt-iter 100 9))


;; Version of newton's method using the change in guess from
;; iteration to iteration instead of absolute tolerance
;; Previous fails for large and small numbers
;; (exact->inexact (sqrt-iter (/ 1 1000) (/ 1 1000)))
;; (exact->inexact (sqrt-iter 1111 (square 123456789)))
(define (newton guess x)
  (newton-helper guess (* 100 guess) x))

(define (newton-helper guess previous x)
  (if (< (/ (abs (- guess previous)) guess) 0.001)
      guess
      (newton-helper (improve guess x) guess x)))

;; Newton's method for cube roots
;; (x/y^2 + 2*y) / 3 is better approximation than y for a given x
(define (cubic-iter guess x)
  (cubic-helper guess (* 100 guess) x))

(define (cubic-helper guess previous x)
  (if (close-enough? guess previous 0.001)
      guess
      (cubic-helper (improve-cubic guess x) guess x)))

(define (close-enough? guess previous tol)
  (if (< (/ (abs (- guess previous)) guess) tol) true false))

(define (improve-cubic guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
