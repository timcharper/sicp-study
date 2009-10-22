#lang scheme/base
;; Implement a procedure that takes f, a, b, and n, and returns the value of the integral.  Simpson's rule
;; http://en.wikipedia.org/wiki/Simpson%27s_rule#Composite_Simpson.27s_rule
;; Simpsons rule:
;; a, b = range
;; n = fidelity operator - more intense computation, more accurate
;; h = (b - a) / n   <- in other words, the incrementer
;; k = 0..(b-a)
;; y(k) = f(a + kh)

(define (cube x) (* x x x))
(define (incrementor step) (lambda (k) (+ k step)))
(define (sum function a b inc-function)
  (if (> a b)
      0
      (+ (function a)
         (sum function (inc-function a) b inc-function))))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0)) b (incrementor dx))
     dx))

(define (simpsons-integral-coefficient k)
  (if (even? k) 2 4)
  )

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-inside-function f a h)
    (lambda (k)
      (*
       (simpsons-integral-coefficient k)
       (f (+ a (* k h))))))
  (*
   (/ h 3)
   (+ (f a) (f b) (sum (simpsons-inside-function f a h) 1 (- n 1) (incrementor 1)))
   )
  )

;; i f(x)_a_b = x^3 dx
;;  (1/4 b^4) - (1/4 a^4)
;;  (1/4 * 81) - (1/4 * 0)
;;  (81 / 4)

(integral cube 0 3 0.01)
(simpsons-integral cube 0 3 100)
(integral cube 0 3 0.1)
(simpsons-integral cube 0 3 10)
(integral cube 0 3 0.25)
(simpsons-integral cube 0 3 4)

(display "----- linear ----\n")
(define (linear x) (+ 0 x))
(integral linear 0 10 0.01)
(simpsons-integral linear 0 10 4)
