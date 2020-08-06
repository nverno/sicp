#lang simply-scheme
;; Exercise 0 - Introduce yourself
;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Exercise 2a - Define can-drive

(define (can-drive x)
  (if (< x 16) '(not yet)
      '(Good to go)))

;; Exercise 2b - Define fizzbuzz

(define (divisible? x y)
  (zero? (remainder x y)))

(define (fizzbuzz x)
  (cond
   ((divisible? x 3)
    (if (divisible? x 5)'fizzbuzz 'fizz))
   ((divisible? x 5) 'buzz)
   (#t x)))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if

#|
Your answer here

|#
