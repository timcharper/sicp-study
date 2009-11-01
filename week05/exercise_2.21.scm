#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil '())

(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))
      ))

(check-equal?
 (square-list '(2 4 6))
 '(4 16 36)
 )

(define (square-list-map items)
  (map square items))

(check-equal?
 (square-list-map '(2 4 6))
 '(4 16 36)
 )
