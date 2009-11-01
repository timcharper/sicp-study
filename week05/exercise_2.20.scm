#lang scheme/base

(require (planet schematics/schemeunit:3))

;; This is one way to implement filter (longer, but iterative)
;; (define (filter f l)
;;   (define (filter-iter f source index newlist)
;;     (cond ((< index 0)
;;            newlist
;;            )
;;           ((f (list-ref source index))
;;            (filter-iter f source (- index 1) (cons (list-ref source index) newlist))
;;            )
;;           (else (filter-iter f source (- index 1) newlist))
;;           )
;;     )
;;   (filter-iter f l (- (length l) 1) '())
;;   )

(define nil '())

;; This is filter defined recursively
(define (filter f l)
  (if (null? l)
      nil
      (if (f (car l))
          (cons (car l) (filter f (cdr l)))
          (filter f (cdr l))
          )
      )
  )

(check-equal?
 (filter odd? '(1 2 3 4))
 '(1 3)
 )
(check-equal?
 (filter even? '(1 2 3 4))
 '(2 4)
 )

(define (same-parity num . l)
  (filter
   (if (odd? num) odd? even?)
   (cons num l)
   )
  )

(check-equal?
 (same-parity 1 2 3 4 5 6 7)
 '(1 3 5 7)
 )

(check-equal?
 (same-parity 2 3 4 5 6 7)
 '(2 4 6)
 )

