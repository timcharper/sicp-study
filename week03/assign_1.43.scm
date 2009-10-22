#lang scheme/base
;; create repeated

(require (planet schematics/schemeunit:3))

(define (compose f g)
  (lambda (x) (f (g x)))
  )
(define (square x) (* x x))

((compose square square) 2)

(define (repeated f n)
  (if (zero? n)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
  )

(check-eq?
 ((repeated square 2) 5)
 (square (square 5))
 )
