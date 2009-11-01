#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil '())
(define (for-each-leaf f l)
  (cond ((null? l) nil)
        ((not (pair? l)) (f l))
        (else (for-each-leaf f (car l))
              (for-each-leaf f (cdr l))
              )
        )
  )

(define (fringe l)
  (let ((output nil))
    (for-each-leaf (lambda (x) (set! output (cons x output))) l)
    (reverse output)
    )
  )

(define x (list (list 1 2) (list 3 4)))

(check-equal?
 (fringe x)
 '(1 2 3 4)
 )

(check-equal?
 (fringe (list x x))
 '(1 2 3 4 1 2 3 4)
 )
