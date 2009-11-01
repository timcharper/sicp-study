#lang scheme/base

(require (planet schematics/schemeunit:3))

;; ex 2.17
(define (last-pair l)
  (if (equal? (cdr l) '())
      l
      (last-pair (cdr l))
      )
  )

(check-equal?
 (last-pair (list 23 72 149 34))
 '(34)
 )

;; ex 2.18

(define (reverse l)
  (define (reverse-builder l newlist)
    (if (equal? (cdr l) '())
        (cons (car l) newlist)
        (reverse-builder (cdr l) (cons (car l) newlist))
        )
    )
  (reverse-builder l '())
  )

;; (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 '())))))
;; (cons 25 (cons 16 (cons 9 (cons 4 (cons 1 '())))))

;; (reverse-builder '(1 2 3) '())
;; (reverse-builder '(2 3) (cons 1 '()))
;; (reverse-builder '(3) (cons 2 (cons 1 '())))
;; (cons 3 (cons 2 (cons 1 '())))

(check-equal?
 (reverse (list 1 4 9 16 25))
 (list 25 16 9 4 1)
 )

