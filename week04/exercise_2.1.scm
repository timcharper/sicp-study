#lang scheme/base

(require (planet schematics/schemeunit:3))

;; * Exercise 2.1. Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (if (negative? d)
      (make-rat (* n -1) (* d -1))
      (cons n d))
  )

(check-equal?
 (make-rat 1 2)
 (make-rat -1 -2)
 )
(check-equal?
 (make-rat 1 2)
 (make-rat -1 -2)
 )

