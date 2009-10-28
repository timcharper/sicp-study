#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (count-factors factor number)
  (define (count-factors-iter factor number result)
    (if (zero? (remainder number factor))
        (count-factors-iter factor (/ number factor) (+ 1 result))
        result
        )
    )
  (count-factors-iter factor number 0)
  )
(define (curry f a) (lambda args (apply f a args)))
(define car (curry count-factors 2))
(define cdr (curry count-factors 3))

(check-equal? (car (cons 5 9)) 5)
(check-equal? (cdr (cons 5 9)) 9)
