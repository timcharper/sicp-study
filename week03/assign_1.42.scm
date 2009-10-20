#lang scheme/base

                                        ; differential of a function - the slope of the curve at a given point

(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.000001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

((deriv cube) 5) ; 75.00001501625775
(sqrt 10)
