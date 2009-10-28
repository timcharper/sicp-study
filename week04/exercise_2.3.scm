#lang scheme/base

(require (planet schematics/schemeunit:3))

;; * Exercise 2.1. Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-dimension w h) (cons w h))
(define (width-dimension dimension) (car dimension))
(define (height-dimension dimension) (cdr dimension))


(define (make-rect x y w h) (cons (make-point x y) (make-dimension w h)))
(define (point-rect rect) (car rect))
(define (dimension-rect rect) (cdr rect))
(define (width-rect rect) (width-dimension (dimension-rect rect)))
(define (height-rect rect) (height-dimension (dimension-rect rect)))
(define (perimeter-rect rect) (+ (* 2 (width-rect rect)) (* 2 (height-rect rect))))
(define (area-rect rect) (* (width-rect rect) (height-rect rect)))

(let ((r (make-rect 0 0 20 10)))
  (check-equal?
   (perimeter-rect r)
   (+ 20 20 10 10)
   )
  (check-equal?
   (area-rect r)
   (* 20 10)
   )
  )
