#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil '())

(define (for-each f l)
  (cond ((null? l) true)
        (else (f (car l))
              (for-each f (cdr l))
              )
        )
  )

(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))

(let ((output nil))
  (for-each (lambda (x) (set! output (cons x output))) (list 57 321 88))
  (check-equal? output '(88 321 57))
  )

