;; GCD
;; if r is the remainder of a/b, then the common divisors of a and b
;; are the same as the common divisors of b and r
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


