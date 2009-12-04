#lang scheme/base

(require (planet schematics/schemeunit:3))

;; * Exercise 2.60. We speciﬁed that a set would be represented as a list with no duplicates. Now suppose
;; we allow duplicates. For instance, the set
;; {1, 2, 3} could be represented as the list (2 3 2 1 3 2 2).
;; Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on
;; this representation. How does the eﬃciency of each compare with the corresponding procedure for the non-
;; duplicate representation? Are there applications for which you would use this representation in preference
;; to the non-duplicate one?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define union-set append)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(check-true (element-of-set? 1 '(1 2 3)))
(check-false (element-of-set? 4 '(1 2 3)))
(check-equal? (adjoin-set 4 '(1 2 3)) '(4 1 2 3))
(check-equal? (adjoin-set 2 '(1 2 3)) '(2 1 2 3))

(check-equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-equal? (union-set '(1 2 3) '(3 4 5)) '(1 2 3 3 4 5))

(check-equal? (intersection-set '(1 3 4 1) '(2 1 3)) '(1 3 1))

;; adjoin-set and union-set are easier, the others stay the same.
