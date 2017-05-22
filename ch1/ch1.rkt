#lang racket 
(require sicp)

;;; Chapter 1, some exers
;; (define (abs x)
;;   (if (< x 0)
;;       (- x)
;;       x))

;; square function
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; (define (f a)
;;   (sum-of-squares (+ a 1) (* a 2)))

;; -------------------------------------------------------------------
;;; 1.3
;; Return the sum of the squares of the largest two parameters
;; (define (sss x1 x2 x3)
;;   (let ((sorted (sort (list x1 x2 x3) >)))
;;     (+ (* (car sorted) (car sorted))
;;        (* (car (cdr sorted)) (car (cdr sorted))))))

(define (sss2 x1 x2 x3)
  (if (> x1 x2)
      (if (> x3 x2)
          (sum-of-squares x1 x3)
          (sum-of-squares x1 x2))
      (if (> x3 x1)
          (sum-of-squares x2 x3)
          (sum-of-squares x1 x2))))

(sss2 1 2 3)
;; (sss 1 2 3)  ; => 13
(sss2 3 1 2)
;; (sss 3 1 2)  ; => 13

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


;; (define (p) (p))
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
;; (define (square x) (* x x))

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

;; Properties of golden ratio, ϕ, which also apply to γ, (1 - √5)/2
;; 1/ϕ + 1 = ϕ, ϕ² = ϕ + 1
;;
;; Show base cases for n=0,1,2
;; Induction on (n+1)th case, assuming both
;;  	fib(n) = (ϕⁿ - γⁿ)/√5
;;  	fib(n-1) = (ϕ^(n-1) - γ^(n-1))/√5
;; then, it follows that
;; 	fib(n+1) = (ϕ^(n+1) - γ^(n+1))/√5

;; Reduction from recurrence relationship:
;; fib(n+1) 	= fib(n) + fib(n-1)
;;		= ( (ϕⁿ - γⁿ) + (ϕ^(n-1) - γ^(n-1)) )/√5
;;		= ( ϕ^(n+1)*ϕ^(-1)*ϕ^(-2) + γ^(n+1)*γ^(-1)*γ^(-2) )/√5
;; ==> using, ϕ² = ϕ + 1
;;		= ( ϕ^(n+1)*ϕ^(-1)*(1+ϕ^(-1)) + γ^(n+1)*γ^(-1)*(1+γ^(-1)) )/√5
;; ==> using, 1/ϕ + 1 = ϕ )
;;		= ( ϕ^(n+1)*ϕ^(-1)*ϕ + γ^(n+1)*γ^(-1)*γ )/√5
;;		= ( ϕ^(n+1) - γ^(n+1) )/√5


;; -------------------------------------------------------------------
;;; Ex. 1.15
;; Givens:
;; - approximation sin x ≈ x for sufficiently small x
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))         ;0.1 considered sufficiently small here
      angle
      (p (sine (/ angle 3.0)))))

(define (sine-trace angle)
  (if (not (> (abs angle) 0.1))
      angle
      (begin
        (set! sine-count (+ 1 sine-count))
        (p (sine-trace (/ angle 3.0))))))

(sine 12.15)
(sin 12.15)

(define sine-count 0)                   ;terrible
(sine-trace 12.15)                      ;5
(sine-trace 100)
(sine-trace 1000)
(sine-trace 10000)

;; a. 5
;; b. O(log a) ==> base 3

;; -------------------------------------------------------------------
;;; Exponentiation
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


;; -------------------------------------------------------------------
;;; Exercise 1.16: iterative version of successive squaring
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


;; -------------------------------------------------------------------
;;; Exercise 1.17, 1.18
;; Multiplication by addition:
;; (define (mult a b)
;;   (if (= b 0)
;;       0
;;       (+ a (mult a (- b 1)))))
;; Devise an integer multiplication by addition function that uses log number of steps
;; (by doubling and halving)
(define (mult n m)
  (mult-iter n m 0))

;; (define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (mult-iter n m acc)
  (cond ((or (= n 0) (= m 0)) acc)
	((= m 1) (+ acc n))
	((even? m) (mult-iter (double n)
                              (halve m)
                              acc))
	(else (mult-iter n
                         (- m 1)
                         (+ acc n)))))

;; -------------------------------------------------------------------
;;; Ex. 1.19: fast fibonacci
;; compute fibonacci numbers in log steps
;; write T_pq as:
;; [ q+p q ]
;; [  q  p ]
;; so, [a b] %*% T_pq => [a(q+p)+bq, aq+bp]
;; Then, compute T_p'q' = (T_pq)^2
;; T_p'q' = [ (q+p)^2+q^2 , q(q+p)+qp ]
;;          [ q(q+p)+qp   , q^2+p^2   ]
;; thus, p' = q^2+p^2, q' = q^2 + 2pq
(define (fib2 n)
  (fib-iter2 1 0 0 1 n))

(define (fib-iter2 a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter2 a
		    b
		    (+ (* p p) (* q q))
		    (+ (* q q) (* 2 p q))
		    (/ count 2)))
	(else (fib-iter2 (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))

;; -------------------------------------------------------------------
;;; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Ex 1.20

;; Normal-order, jesus
;; 
;; 1. +0 evaluations
;; (if (= 40 0)
;;     206
;;     (gcd (remainder 206 40)))
;;
;; 2. +1 evaluations (first if clause)
;; (if (= (remainder 206 40) 0)  ;6
;;     (remainder 206 40)
;;     (gcd (remainder 206 40)
;;          (remainder 40 (remainder 206 40))))
;;
;; 3. +1 evals
;; (if (= (remainder 40 (remainder 206 40)) 0) ;4
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder 40 (remainder 206 40))
;;          (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))))
;;
;; 4. +1 evals
;; (if (= (remainder (remainder 206 40)
;;                   (remainder 40 (remainder 206 40))) ;2
;;        0)
;;     (remainder (remainder 206 40)
;;                (remainder 40 (remainder 206 40)))
;;     (gcd (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))
;;          (remainder (remainder 40 (remainder 206 40))
;;                     (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40))))))
;;
;; 5. +14 evals
;; (if (= (remainder (remainder 40 (remainder 206 40))
;;                   (remainder (remainder 206 40)
;;                              (remainder 40 (remainder 206 40)))) ;0
;;        0)
;;     (remainder (remainder 40 (remainder 206 40))
;;                (remainder (remainder 206 40)
;;                           (remainder 40 (remainder 206 40))))
;;     (gcd ...))                          ;not evaluated
;;
;; I think the total calls to remainder for normal-order is then
;; 17.
;;
;; Applicative Order Evaluation
;;
;; 1. +1 evals
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))           ;6
;;
;; 2. +1 evals
;; (if (= 6 0)
;;     40
;;     (gcd 6 (remainder 40 6)))           ;4
;;
;; 3. +1 evals
;; (if (= 4 0)
;;     6
;;     (gcd 4 (remainder 6 4)))              ;2
;;
;; 4. +1 evals
;; (if (= 2 0)
;;     4
;;     (gcd 2 (remainder 4 2)))            ;0
;;
;; 5. +0 evals
;; (if (= 0 0)
;;     2
;;     (gcd ...))                          ;not evaluated
;;
;; Totaling 4 calls to remainder for applicative-order.

;; -------------------------------------------------------------------
;;; Primes

;; find smallest integral divisor (> 1)
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;; testing if number is prime -- only prime if its smallest divisor is itself
;; only needs test numbers up to sqrt(n) => O(sqrt(n))
(define (prime? n)
  (= (smallest-divisor n) n))

;; (prime? 99)
;; (prime? 101)

;;; Fermat test
;; O(log(n)) -- fermat's little theorem
;; If n is a prime number and a is any positive integer less than n,
;; then a raised to the nth power is congruent to a mod n.

;; so, to test primality of number n, pick a random number, a<n, and compute
;; a^n mod n. If result is not equal to a, then number is not prime. If it is,
;; then chances are good that n is prime (fails in some cases -- eg. carmichael)

;; modexp
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ;; use successive squaring, so O(log(n))
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; (fermat-test 101)
;; (fermat-test 100)

;; run test a given number of times to increase the accuracy
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 1009 10)
(fast-prime? 1003 10)

;; -------------------------------------------------------------------
;;; Ex. 1.21
(map smallest-divisor '(199 1999 19999))

;; -------------------------------------------------------------------
;;; Ex. 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  ;; runtime defined in sicp module
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; report first three primes in range [start, end)
(define (search-for-primes-in-range start end count)
  (when (and (< start end)
             (< count 3))
    (if (even? start)
        (search-for-primes-in-range (+ 1 start) end count)
        (begin
          (if (prime? start)
              (begin 
                (timed-prime-test start)
                (search-for-primes-in-range (+ 2 start) end (+ 1 count)))
              (search-for-primes-in-range (+ 2 start) end count))))))

(define (search-for-primes start)
  (search-for-primes-in-range start (+ start 1000) 0))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)

;; -------------------------------------------------------------------
;;; Ex. 1.23

;; return 3 if n is 2, otherwise n+2
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

;; find smallest integral divisor (> 1)
(define (find-divisor-next n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;; use `next' instead of 1 + test-divisor
        ;; to avoid needlessly checking larger even numbers
        (else (find-divisor-next n (next test-divisor)))))


;; -------------------------------------------------------------------
;;; Ex. 1.24
;; use `fast-prime?', the fermat method instead of `prime?'
;; should see log(n) growth instead of sqrt(n)

(define (timed-prime-test-fermat n)
  (newline)
  (display n)
  ;; runtime defined in sicp module
  (start-prime-test-fermat n (runtime)))
(define (start-prime-test-fermat n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))))

;; report first three primes in range [start, end)
(define (search-for-primes-in-range-fermat start end count)
  (when (and (< start end)
             (< count 3))
    (if (even? start)
        (search-for-primes-in-range-fermat (+ 1 start) end count)
        (begin
          (if (fast-prime? start 3)
              (begin 
                (timed-prime-test-fermat start)
                (search-for-primes-in-range-fermat (+ 2 start) end (+ 1 count)))
              (search-for-primes-in-range-fermat (+ 2 start) end count))))))

(define (search-for-primes start)
  (search-for-primes-in-range start (+ start 1000) 0))

(search-for-primes-fermat 1000)
(search-for-primes-fermat 10000)
(search-for-primes-fermat 100000)
(search-for-primes-fermat 1000000)

;; -------------------------------------------------------------------
;;; Ex. 1.25

;; No, the modulus should be taken at each iteration to ensure the numbers
;; don't grow too large.

;; -------------------------------------------------------------------
;;; Ex. 1.26

;; `expmod' is being called twice during each recursion, thus ending up
;; with a runtime of 2^log_2(n) ==> O(n) runtime

;; -------------------------------------------------------------------
;;; Ex. 1.27

;; Try fermat's test on some Carmichael numbers:
(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(map fermat-test carmichael-numbers)    ;all #t
(map prime? carmichael-numbers)         ;all #f
