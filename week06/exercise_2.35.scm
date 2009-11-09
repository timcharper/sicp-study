#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil null)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;  (define (count-leaves t)
;;    (accumulate <??> <??> (map <??> <??>)))

;; recursive:
(define (count-leaves t)
  (accumulate
   (lambda (value count)
     (+ count (if (list? value) (count-leaves value) 1)))
   0
   t
   )
  )

;; Using map

(define (flatten l)
  (cond
   ((null? l) '())
   ((list? l) (append (flatten (car l)) (flatten (cdr l))))
   (else (list l))
   )
  )

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (flatten t))))

(check-equal?
 (count-leaves '(1 (2 (3 4))))
 4
 )

(check-equal?
 (count-leaves '((1 2) (3 4)))
 4
 )
