;; Testing for primality
;; Method 1: searching for divisors O(sqrt(n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (reduce func list)
  ;; (assert (not (null? list)))
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))
(define (boolean->integer b)
  (case b
    ((#f) 0)
    ((#t) 1)))
(reduce + (map boolean->integer (map prime? (sequence->list 100))))

;; Fermat test
;; O(log n) primality test based on Fermat's Little Theorem
;; Fermat's Little Theorem: If n is a prime number and a is any positive integer
;; less than n, then a raised to the nth power is congruent to a modulo n.
;; (congruent modulo n means they both have the same remainder modulo n)
;;
;; Note: There are cases where this will work for non-primes, so
;; in order to increase confidence that a number is prime, mutiple tests are tried.

;; Compute exponential modulo m (using successive squaring)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

;; Fermat test: choose random number (1, n-1] and check for
;; equality
;; Note: there are some extremely rare numbers n that have the property
;; a^n is congruent to a modulo n for all integers a<n, thus fooling the
;; fermat test.
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; Exercise 1.21
(map smallest-divisor (list 199 1999 19999))

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
