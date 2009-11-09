#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x)))
   nil
   sequence))

(check-equal? (reverse '(1 2 3)) '(3 2 1))


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(check-equal? (reverse '(1 2 3)) '(3 2 1))

