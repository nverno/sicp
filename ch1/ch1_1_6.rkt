;;; First Chapter exercises
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
