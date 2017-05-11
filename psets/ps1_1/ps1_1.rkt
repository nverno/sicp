;;; This is the code for ps1.

(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a) (p2 b))))

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle x)))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))

(define (comb n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))

;; Exercise 11: curried application
(define foo1
  (lambda (x)
    (* x x)))
(+ (foo1 1) (foo1 1) (foo1 1))

(define foo2
  (lambda (x y)
    (/ x y)))
(foo2 6 2)

(define foo3
  (lambda (x)
    (lambda (y)
      (/ x y))))

(define foo4
  (lambda (x)
    (x 3)))

(define foo5
  (lambda (x)
    (cond ((= x 2)
	   (lambda () x))
	  (else
	   (lambda () (* x 3))))))

(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))
