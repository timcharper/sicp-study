#lang scheme/base

(require (planet schematics/schemeunit:3))

(define nil null)
(define (enumerate-interval a b)
  (define (enumerate-interval-iter a b l)
    (if (> a b)
        l
        (enumerate-interval-iter a (- b 1) (cons b l))
        )
    )
  (enumerate-interval-iter a b nil)
  )

(check-equal? (enumerate-interval 1 3) '(1 2 3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (j) (map
                (lambda (i) (cons j i))
                (enumerate-interval j n)))
   (enumerate-interval 1 n))
  )

(check-equal?
 (unique-pairs 3)
 '((1 . 1) (1 . 2) (1 . 3) (2 . 2) (2 . 3) (3 . 3)))

(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (prime-sum-pairs n)
  (filter prime-sum? (unique-pairs n))
  )

(check-equal?
 (prime-sum-pairs 5)
 '((1 . 1) (1 . 2) (1 . 4) (2 . 3) (2 . 5) (3 . 4))
 )
