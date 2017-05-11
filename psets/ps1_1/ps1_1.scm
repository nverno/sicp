;;; This is the code for ps1.

(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a b) (p2 a b))))

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
  (/ (fact n)
     (* (fact k) (fact (- n k)))))

(comb 10 2)

;;; Ex 11: curried => 3
(define foo1
  (lambda (x)
    (* x x)))

(foo1 (foo1 (expt 3 .25)))

(define foo2
  (lambda (x y)
    (/ x y)))

(foo2 (foo2 12 2) (foo2 4 2))

(define foo3
  (lambda (x)
    (lambda (y)
      (/ x y))))

((foo3 12) ((foo3 16) 4))

(define foo4
  (lambda (x)
    (x 3)))

(foo4 (foo4 (lambda (x) +)))

(define foo5
  (lambda (x)
    (cond ((= x 2)
           (lambda () x))
          (else
           (lambda () (* x 3))))))

(+ ((foo5 ((foo5 2)))) 1)

(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))

((foo6 list) identity)
