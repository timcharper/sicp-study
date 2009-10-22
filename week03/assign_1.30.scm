#lang scheme/base

(define (sum function a b inc-function)
  (if (> a b)
      0
      (+ (function a)
         (sum function (inc-function a) b inc-function))))

(define (sum-iter term a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))
        ))
  (iter a 0))

(define (square x) (* x x))
(define (incrementer amount) (lambda (i) (+ i amount)))

(sum square 0 3 (incrementer 1))
(sum-iter square 0 3 (incrementer 1))
