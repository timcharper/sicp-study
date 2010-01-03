#lang scheme/base

(require (planet schematics/schemeunit:3))

;; A. Explain what was done above. Why can’t we assimilate the predicates number? and variable? into the data-directed dispatch?
;;
;; We turned it into using a data dispatch approach. We can't assimilate the predicates number? and variable? Because they have no operator associated with them.
;;
;; B.

;; Operation, type -> procedure
;; Dispatch table.
;;
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;;; --------------------------------------------

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
;; Two variables are the same if the symbols representing them are eq?:

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Sums and products are constructed as lists:
(define (make-subtract s1 s2)
  (cond ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (- s1 s2))
        (else (list '- s1 s2))))

;; A sum is a list whose ﬁrst element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; A product is a list whose ﬁrst element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;  Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule:
;; $ \frac{d(u^n)}{dx} = nu^{n - 1}(\frac{du}{dx}) $
;; by adding a new clause to the deriv program and deﬁning appropriate procedures exponentiation?, base,
;; exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in
;; the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

(define (exponentiation? exponentiation)
  (and (pair? exponentiation) (eq? (car exponentiation) '**))
  )

(define (base exponentation) (car exponentation))
(define (exponent exponentation) (cadr exponentation))

(define (addend s) (car s))
(define (augend s) (cadr s))

(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (install-deriv-package)
  (put 'deriv '+ (lambda (operands var)
                   (make-sum (deriv (addend operands) var)
                             (deriv (augend operands) var))))
  (put 'deriv '* (lambda (operands var)
                   (make-sum
                    (make-product (multiplier operands)
                                  (deriv (multiplicand operands) var))
                    (make-product (deriv (multiplier operands) var)
                                  (multiplicand operands)))))
  (put 'deriv '** ( lambda (operands var)
                     (make-product
                      (exponent operands)
                      (make-exponentiation (base operands) (make-subtract (exponent operands) 1))))))
(install-deriv-package)

(check-equal? (make-exponentiation 2 8) 256)
(check-equal? (make-exponentiation 'x 0) 1)
(check-equal? (make-exponentiation 'x 1) 'x)
(check-equal? (make-exponentiation 1 'x) 1)
(check-equal? (make-exponentiation 'x 3) '(** x 3))

(check-true (exponentiation? '(** x 2)))
(check-false (exponentiation? '(+ x 2)))

(check-equal? (base '(2 4)) 2)
(check-equal? (exponent '(2 4)) 4)

(check-equal? (make-subtract 4 2) 2)
(check-equal? (make-subtract 'x 2) '(- x 2))
(check-equal? (make-subtract 'x 0) 'x)
(check-equal? (make-subtract 0 'x) '(- 0 x))

(check-equal?
 (deriv '(+ x 3) 'x)
 '1
 )

(check-equal?
 (deriv '(* x y) 'x)
 'y)

(check-equal?
 (deriv '(* (* x y) (+ x 3)) 'x)
 '(+ (* x y) (* y (+ x 3))))
(check-equal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))
