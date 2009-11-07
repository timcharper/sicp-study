#lang scheme/base

(require (planet schematics/schemeunit:3))

;; 2.33

(define nil null)

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(check-equal?
 (accumulate + 0 (list 1 2 3 4 5))
 15)

;; Map Function using accumulate

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(check-equal? 
 (map square '(1 2 3))
 '(1 4 9)
 )

;; Append using accumulate

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(check-equal?
 (append '(1 2 3) '(4 5 6))
 '(1 2 3 4 5 6)
 )

;; length using accumulate

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(check-equal?
 (length '(1 2 3 4 5 6))
 6
 )

