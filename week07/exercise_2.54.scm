#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (list-equal? left right)
  (cond
   [(and (pair? left) (pair? right))
     (and (list-equal? (car left) (car right)) (list-equal? (cdr left) (cdr right)))
     ]
   [else (eq? left right)]
   )
  )

(check-true (list-equal? '(1) '(1)))
(check-true (list-equal? '(1 2) '(1 2)))
(check-true (list-equal? '(1 2 (3 4)) '(1 2 (3 4))))
(check-true (list-equal? 1 1))
(check-false (list-equal? 1 2))
(check-false (list-equal? '(1 2) '(1 (2))))
(check-false (list-equal? '(1 2) '(1 3)))
