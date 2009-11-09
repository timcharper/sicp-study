#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;  Exercise 2.34. Evaluating a polynomial in x at a given value of x can be formulated as an accumulation.
;; Fill in the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume
;; that the coefficients of the polynomial are arranged in a sequence, from a0 through an .

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(let ((x 2))
  (check-equal?
   (horner-eval x (list 1 3 0 5 0 1))
   (+ 1 (* 3 x) (* 5 (expt x 3)) (expt x 5)))
  )

(define nil null)


