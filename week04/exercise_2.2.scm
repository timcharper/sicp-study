#lang scheme/base

(require (planet schematics/schemeunit:3))

;; * Exercise 2.1. Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (make-point x y)
  (cons x y)
  )

(define (make-line point1 point2)
  (cons point1 point2)
  )

(define (start-line line) (car line))
(define (end-line line) (cdr line))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (add-points point1 point2)
  (make-point (+ (car point1) (car point2)) (+ (cdr point1) (cdr point2)))
  )

(define (divide-points point1 point2)
  (make-point (/ (car point1) (car point2)) (/ (cdr point1) (cdr point2)))
  )

(define (midpoint-line line)
  (divide-points
   (add-points (start-line line) (end-line line))
   (make-point 2 2)
   )
  )

(check-equal?
 (midpoint-line (make-line (make-point 10 10) (make-point 20 30)))
 (make-point 15 20)
 )
