#lang scheme/base

(require (planet schematics/schemeunit:3))



(define l1 (list 1 3 (list 5 7) 9))
(check-equal?
 (car (cdr (car (cdr (cdr l1)))))
 7
 )

(define l2 (list (list 7)))
(check-equal?
 (car (car l2))
 7
 )

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(check-equal?
 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
 7
 )
