(defpackage :ch1
  (:use :cl))
(in-package :ch1)

;; How many ways to make change (US change) for given AMOUNT?
;; using recursive method ==> tree-recursive
(defun count-change (amount)
  (labels ((first-denomination (kinds-of-coins)
             (cond ((= kinds-of-coins 1) 1)
                   ((= kinds-of-coins 2) 5)
                   ((= kinds-of-coins 3) 10)
                   ((= kinds-of-coins 4) 25)
                   ((= kinds-of-coins 5) 50)))
           (cc (amount kinds-of-coins)
             (cond ((= amount 0) 1)
                   ((or (< amount 0)
                        (= kinds-of-coins 0))
                    0)
                   (t (+ (cc amount
                             (- kinds-of-coins 1))
                         (cc (- amount
                                (first-denomination kinds-of-coins))
                             kinds-of-coins))))))
    (cc amount 5)))

;; try it out
(count-change 100)                      ;292

;;; Memoized version
(defun count-change-2 (amount coins)
  (let ((table ()))
    (loop :for i :from 1 :to amount
       (loop :for coin :in coins
          :if (> coin (length table))
          ))))
