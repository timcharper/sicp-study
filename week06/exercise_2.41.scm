#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (integer! n) (inexact->exact (truncate n)))
(check-equal?
 (integer! 1.5)
 1)

(define nil null)
(define (enumerate-interval a b)
  (define (enumerate-interval-iter a b l)
    (if (> a b)
        l
        (enumerate-interval-iter a (- b 1) (cons b l))
        )
    )
  (enumerate-interval-iter a (integer! b) nil)
  )

(check-equal? (enumerate-interval 1 3) '(1 2 3))
(check-equal? (enumerate-interval 3 1) '())
(check-equal? (enumerate-interval 1 (/ 3 2)) '(1))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (distinct-triplets-with-sum n x)
  (flatmap (lambda (j) (map
                    (lambda (k) (let ((l (- x k))) (list j k l)))
                    (enumerate-interval j (min (/ (- x j) 2) x))))
       (enumerate-interval 1 n)
       )
  )

(check-equal?
 (distinct-triplets-with-sum 3 6)
 '((1 1 5) (1 2 4) (2 2 4))
 )

;; n=3 x=5
;; 1 2 3
;;      
;; n=5 x=10
;;      
;; j k l
;;      
;; k <= x - j - 1 - k
;; 2k <= x - j - 1
;; k <= (x - j - 1) / 2
;;      
;; k <= 5 - 1 - k
;; 2k <= 4
;; k <= 2
;; ;; ---
;; k >= j
