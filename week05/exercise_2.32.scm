#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest))
        )
      )
  )

(subsets '(1 2 3))
(let ((rest (subsets '(2 3))))
  (append rest (map (lambda (x) (cons 1 x)) rest))
  ) ;; 1
(let ((rest (subsets '(3))))
  (append rest (map (lambda (x) (cons 2 x)) rest))
  ) ;; 2
(let ((rest (list nil)))
  (append rest (map (lambda (x) (cons 3 x)) rest))
  ) ;; 3 => ((() (3)))
(append '(() (3)) (map (lambda (x) (cons 2 x)) '(() (3)))) ;; 2 => '(() (3) (2) (2 3))
(append '(() (3) (2) (2 3)) (map (lambda (x) (cons 1 x)) '(() (3) (2) (2 3)))) ;; 1 => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

