;; Simple recursive solution (linear recursive process and O(n) space)
(define (expn b n)
  (if (= n 0)
      1
      (* b (expn b (- n 1)))))

;; linear iteration (also O(1) space)
(define (expn2 b n)
  (define (exp-iter counter product)
    (if (= counter 0)
	product
	(exp-iter (- counter 1) (* b product))))
  (exp-iter n 1))

;; Successive squaring (O(log(n)) for space and time)
;; ie, b^2 = b*b, b^4 = b^2 * b^2, b^8 = b^4 * b^4
;; so, only three multiplies
;;
;; b^n = (b^(n/2))^2 if n is even
;; b^n = b*b^(n-1) if n is odd
(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))


;; Exercise 1.16: iterative version of successive squaring
(define (fast-expt2 b n)
  (exp-iter2 1 b n))

(define (exp-iter a b n)
  (cond ((= n 0) a)
	((= n 1) (* a b))
	((even? n) (exp-iter a (* b b) (/ n 2)))
	(else (exp-iter (* a b) b (- n 1)))))

;; or take advantage of x*(x^2)^((n-1)/2) for odd n
(define (exp-iter2 a b n)
  (print ".")
  (cond ((= n 0) a)
	((= n 1) (* a b))
	((even? n) (exp-iter2 a (* b b) (/ n 2)))
	(else (exp-iter2 (* a b) (* b b) (/ (- n 1) 2)))))


;; Exercise 1.17
;; Multiplication by addition:
;; (define (mult a b)
;;   (if (= b 0)
;;       0
;;       (+ a (mult a (- b 1)))))
;; Devise an integer multiplication by addition function that uses log number of steps
;; (by doubling and halving)
(define (mult n m)
  (mult-iter n m 0))

(define (mult-iter n m acc)
  (cond ((or (= n 0) (= m 0)) acc)
	((= m 1) (+ acc n))
	((even? m) (mult-iter (double n) (halve m) acc))
	(else (mult-iter n (- m 1) (+ acc n)))))

(define (double n) (+ n n))
(define (halve n) (/ n 2))
