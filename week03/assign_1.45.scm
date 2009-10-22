#lang scheme/base

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (repeated f n)
  (if (<= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
  )
(define (average a b) (/ (+ a b) 2))

(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess tries)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (if (= tries 0) 'error (try next (- tries 1)))
          )
      )
    )
  (try first-guess 50))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (n-power x n)
  (if (= n 0)
      1
      ((repeated (lambda (v) (* v x)) (- n 1)) x)
      )
  )

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (n-power y 3))))
               1.0))

(define (n-root x power)
  (define (n-root-with-damping x power damping)
    (fixed-point ((repeated average-damp damping) (lambda (y) (/ x (n-power y (- power 1)))))
                 1.0)
    )
  (retry-and-increment-until
   (lambda (result x) (or (not (equal? result 'error)) (>= x 10)))
   1
   (lambda (x) (+ x 1))
   (lambda (damping) (n-root-with-damping x power damping))
   )
  )

(define (retry-and-increment-until test x incrementor body)
  (let ((result (body x)))
    (if (test result x)
        result
        (retry-and-increment-until test (incrementor x) incrementor body)
        )
    )
  )

(define (upto start stop f)
  (retry-and-increment-until
   (lambda (result x) (>= x stop))
   start
   (lambda (x) (+ x 1))
   f
   )
  )

(upto 2 10 (lambda (x)
             (display "n-root: ") (display x) (newline)
             (display "result: ") (display (n-root (n-power 2 x) x)) (newline)
             (newline)
             ))

;; (n-root (n-power 2 3) 3)
;; (n-root (n-power 2 4) 4)
;; (n-root (n-power 2 5) 5)
;; (n-root (n-power 2 5) 5)
