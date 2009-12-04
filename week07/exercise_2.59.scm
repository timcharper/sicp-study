#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(check-true (element-of-set? 1 '(1 2 3)))
(check-false (element-of-set? 4 '(1 2 3)))
(check-equal? (adjoin-set 4 '(1 2 3)) '(4 1 2 3))
(check-equal? (adjoin-set 2 '(1 2 3)) '(1 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2)
         )
        (else (union-set (cdr set1) (cons (car set1) set2))))
  )

(check-equal? (union-set '(1 2 3) '(4 5 6)) '(3 2 1 4 5 6))
(check-equal? (union-set '(1 2 3) '(3 4 5)) '(2 1 3 4 5))
