#lang scheme/base
; Implement a procedure that takes f, a, b, and n, and returns the value of the integral.  Simpson's rule
; http://en.wikipedia.org/wiki/Simpson%27s_rule#Composite_Simpson.27s_rule

; Simpsons rule:
; a, b = range
; n = fidelity operator - more intense computation, more accurate
; h = (b - a) / n   <- in other words, the incrementer
; k = 0..(b-a)
; y(k) = f(a + kh)

(define (cube x) (* x x x))
(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a) 
         (sum term (next a) next b)))) 

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx)) 
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))


(define (simpsons-integral-coefficient k)
  (if (even? k) 2 4)
  )
(define (simpsons-integral-loop f a b h k n total)
  (if (= n 0)
      (* (/ h 3) total)
      (simpsons-integral-loop
       f
       a
       b
       h
       (+ k 1)
       (- n 1)
       (+ total (* (simpsons-integral-coefficient k) (f (+ a (* k h)))))
       )
      )
  )

(define (simpsons-integral f a b n)
  (simpsons-integral-loop
   f
   a
   b
   (/ (- b a) n)
   1
   (- n 1)
   (+ (f a) (f b))
   )
  )

(integral cube 0 3 0.01)
(integral cube 0 3 0.1)
(integral cube 0 3 0.25)
(simpsons-integral cube 0 3 100)
(simpsons-integral cube 0 3 10)
(simpsons-integral cube 0 3 4)

; i f(x)_a_b = x^3 dx
;  (1/4 b^4) - (1/4 a^4)
;  (1/4 * 81) - (1/4 * 0)
;  (81 / 4)

(define (linear x) (+ 0 x))
(integral linear 0 10 0.01)
(simpsons-integral linear 0 10 4)

