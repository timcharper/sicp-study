#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;;Alyssa also works out the product of two intervals by finding the minimum and the maximum of the products of the bounds and using them as the bounds of the resulting interval. (Min and max are primitives that find the minimum or maximum of any number of arguments.)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;To divide two intervals, Alyssa multiplies the first by the reciprocal of the second. Note that the bounds of the reciprocal interval are the reciprocal of the upper bound and the reciprocal of the lower bound, in that order.

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;; Exercise 2.7.  Define selectors upper-bound and lower-bound to complete the implementation.

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(check-equal?
 (add-interval (make-interval -5 10) (make-interval 0 5))
 (make-interval -5 15)
 )

;; Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(check-equal?
 (sub-interval (make-interval -5 10) (make-interval 0 5))
 (make-interval -5 5)
 )

;; Exercise 2.9 - show that the width is a function of the widths of the intervals involved when adding, but not for multiplication

(define (width-interval interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

(check-equal? (width-interval (make-interval 0 10)) 5)

(let (
      (interval-a (make-interval 0 10))
      (interval-b (make-interval 10 15))
      )
  (check-equal?
   (width-interval (add-interval interval-a interval-b))
   (+ (width-interval interval-a) (width-interval interval-b))
   )

  (check-not-equal?
   (width-interval (mul-interval interval-a interval-b))
   (* (width-interval interval-a) (width-interval interval-b))
   )
  (check-not-equal?
   (width-interval (div-interval interval-a interval-b))
   (/ (width-interval interval-a) (width-interval interval-b))
   )
  )

;; Exercise 2.10 -  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

(define (spans-zero?-interval interval) (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0)))

(test-true "The interval spans 0" (spans-zero?-interval (make-interval -5 5)))
(test-true "The interval spans 0" (spans-zero?-interval (make-interval 0 5)))
(test-false "The interval doesn't span 0" (spans-zero?-interval (make-interval 5 10)))

(define (div-interval x y)
  (if (spans-zero?-interval y)
      (error "cannot divide by an interval that spans 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      )
  )

(test-exn "div-interval fails when dividing by interval that spans 0"
          (lambda (x) (equal? (exn-message x) "cannot divide by an interval that spans 0"))
          (lambda () (div-interval (make-interval 5 10) (make-interval -5 5)))
          )

(test-not-exn "div-interval succeeds when dividing by interval that doesn't spans 0"
              (lambda () (div-interval (make-interval 5 10) (make-interval 2 5)))
              )

;;Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-center-percent center percent)
  (let ((distance (* center percent)))
    (make-interval (- center distance) (+ center distance))
    )
  )

(check-equal?
 (make-center-percent 100 0.1)
 (make-interval 90.0 110.0)
 )

(define (midpoint-interval interval) (/ (+ (lower-bound interval) (upper-bound interval)) 2))
(define (find-percent-interval interval)
  (- 1 (/ (lower-bound interval) (midpoint-interval interval)))
  )

(check-equal?
 (find-percent-interval (make-interval 90 110))
 (/ 1 10)
 )

;; ex 2.13

;; ex 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(let ((interval-1 (make-center-percent 100 0.1))
      (interval-2 (make-center-percent 120 0.2)))
  (check-not-equal? (par1 interval-1 interval-2) (par2 interval-1 interval-2))
  )

