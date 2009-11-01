#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))



(define ex_24 (list 1 (list 2 (list 3 4))))
(count-leaves ex_24)

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)





