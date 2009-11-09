#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil null)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 4) (1 2 4) (3 5 4)))

(let (
      (s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
      )
  (check-equal? (accumulate-n + 0 s) '(22 26 30))
  )
