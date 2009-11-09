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


;; dot-product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(check-equal?
 (dot-product '(1 2 3 4) '(1 2 3 4))
 (+ (* 1 1) (* 2 2) (* 3 3) (* 4 4))
 )

;; matrix * vector

(define (matrix-*-vector m v)
  (map (lambda (ti)
         (apply + (map * ti v))
         )
       m)
  )

(check-equal?
 (matrix-*-vector '((1 3 5) (2 4 6)) '(1 2 3))
 '(22 28)
 )

;; transpose

(define (transpose mat)
  (accumulate-n cons '() mat))

(check-equal?
 (transpose '((1 3) (2 4)))
 '((1 2) (3 4))
 )

(check-equal?
 (transpose '((1 4 7) (2 5 8) (3 6 9)))
 '((1 2 3) (4 5 6) (7 8 9))
 )

;; matrix * matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (t-row)
                  (apply + (map * row t-row))
                  )
                cols
                )
           )
         m
         )
    ))

(check-equal?
 (matrix-*-matrix '((1 3) (2 4)) '((1 3) (2 4)))
 '((7 15) (10 22))
 )

;; > m <- matrix(1:4, ncol=2, nrow=2)
;; > m %*% m 
;;      [,1] [,2]
;; [1,]    7   15
;; [2,]   10   22

(check-equal?
 (matrix-*-matrix '((1 4 7) (2 5 8) (3 6 9)) '((1 4 7) (2 5 8) (3 6 9)))
 '((30 66 102) (36 81 126) (42 96 150))
 )

;; > m <- matrix(1:9, ncol=3, nrow=3)
;; > m %*% m
;;      [,1] [,2] [,3]
;; [1,]   30   66  102
;; [2,]   36   81  126
;; [3,]   42   96  150


;; '((1 3) (2 4)) '((1 3) (2 4))
;; '((1 3) (2 4)) '((1 2) (3 4))
;; (apply + (map * '(1 3) '(1 2)))
;; (apply + (map * '(1 3) '(3 4)))
;; (apply + (map * '(2 4) '(1 2)))
;; (apply + (map * '(2 4) '(3 4)))
