#lang racket 
(require sicp)

;;; Solve derivatives numerically using case analysis
;; solves taking the derivative of:
;; - constants
;; - vars w/ respect to themselves
;; - sums
;; - products

;; EXP is the expression given, VAR is the variable to take the derivative
;; with respect to
(define (deriv exp var)
  (cond ((constant? exp var) 0)         ;derivative of constant is 0
        ((same-var? exp var) 1)         ;derivative of var w/ respect to var is 1
        ((sum? exp)                     ;d(sum) is sum of deriv of parts
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)                 ;sum of d(first)*second + d(second)*first
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp))))))

;;; determine if an element is atomic
(define (atom? x)
  (not (or (pair? x)
           (null? x))))

;;; determine if an EXP is a constant w/ respect to VAR
(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

;;; determine if an EXP and a VAR are the same thing
(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

;;; determine if an EXP is a sum
(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

;; create a sum expression
;; (define (make-sum a1 a2)
;;   (list '+ a1 a2))

;; sum w/ simplification
(define (make-sum a1 a2)
  (cond [(and (number? a1)
              (number? a2))
         (+ a1 a2)]
        [(and (null? a1) (null? a2))
         nil]
        [(and (number? a1) (zero? a1))
         a2]
        [(and (number? a2) (zero? a2))
         a1]
        [else
         (list '+ a1 a2)]))

;; create some sum accessors
(define a1 cadr)
(define a2 caddr)

;;; Products
(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

;; (define (make-product m1 m2)
;;   (list '* m1 m2))

;;; with simplification
(define (make-product m1 m2)
  (cond [(and (number? m1)
              (number? m2))
         (* m1 m2)]
        [(and (null? m1) (null? m2))
         nil]
        [(number? m1)
         (cond [(zero? m1) 0]
               [(eq? 1 m1) m2])]
        [(number? m2)
         (cond [(zero? m2) 0]
               [(eq? 1 m2) m1])]
        [else (list '* m1 m2)]))

(define m1 cadr)
(define m2 caddr)

;;; Test it out a bit
(define foo    ; a*x*x + b*x + c
  '(+ (+ (* a (* x x))
         (* b x))
      c))

;; examples:  foo = 2*a*x + b 
(deriv foo 'x)                          ; {+ {* a {+ x x}} b}
(deriv foo 'b)                          ; x
(deriv foo 'c)                          ; 1
