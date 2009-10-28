#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (cons a b) (lambda (m) (m a b)))
(define (car s) (s (lambda (a b) a)))
(define (cdr s) (s (lambda (a b) b)))

(check-equal? (car (cons 1 2)) 1)
(check-equal? (cdr (cons 1 2)) 2)
