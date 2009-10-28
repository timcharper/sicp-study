#lang scheme/base

(require (planet schematics/schemeunit:3))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (multiply a b) (lambda (f) (b (a f))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (to-int church)
  (define (to-int-inc x) (+ 1 x))
  ((church to-int-inc) 0))

(check-equal? (to-int zero) 0)
(check-equal? (to-int (add-1 zero)) 1)
(check-equal? (to-int (add-1 (add-1 zero))) 2)

(check-equal? (to-int one) 1)
(check-equal? (to-int two) 2)
(check-equal? (to-int three) 3)

(check-equal? (to-int (multiply three two)) 6)
(check-equal? (to-int (multiply three one)) 3)
(check-equal? (to-int (multiply three zero)) 0)

(check-equal? (to-int (add zero zero)) 0)
(check-equal? (to-int (add one zero)) 1)
(check-equal? (to-int (add two zero)) 2)
(check-equal? (to-int (add three zero)) 3)
(check-equal? (to-int (add three one)) 4)
(check-equal? (to-int (add three two)) 5)
(check-equal? (to-int (add three three)) 6)
