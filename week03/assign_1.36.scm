#lang scheme/base

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

(define (average x y) (/ (+ x y) 2))
(define (sqrt x) 
  (fixed-point (lambda (y) (average y (/ x y))) 
               1.0))

; (fixed-point cos 1.0)
; (/ 10 (/ 10 (/ 10 (/ 10 2.0))))
; (sqrt 10)

(fixed-point (lambda (y) (/ (log 1000) (log y))) 1.01)
; 51 steps

(fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 1.01)
; 19 steps!

