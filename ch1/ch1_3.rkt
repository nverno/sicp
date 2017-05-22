#lang racket
(require sicp)

(define (cube x) (* x x x))

;; -------------------------------------------------------------------
;;; Ex. 1.29

;; higher-order summation procedure, eg. sigma
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; (define (inc n)
;;   (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;; approx. to integral
;; from a to b of definite integral, [f(a + dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2)+
;; ...]dx
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.001)

;; -------------------------------------------------------------------
;;; Ex. 1.29
;; Simpson's Rule:
;; h/3[y0 + y1 + 2y2 + y3 + 2y4 + ... + 2y_(n-2) + y_(n-1) + y_n
;; where h = (b - a)/n for some even integer n,
;; and y_k = f(a + kh)
(define (simpsons-integral f a b n)
  (define (add-dx x)))

