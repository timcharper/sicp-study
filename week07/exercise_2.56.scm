#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp) (make-exponentiation (base exp) (make-subtract (exponent exp) 1)))
         )
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
;; Two variables are the same if the symbols representing them are eq?:

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Sums and products are constructed as lists:
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

(define (make-subtract s1 s2)
  (cond ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (- s1 s2))
        (else (list '- s1 s2))))

;; A sum is a list whose ﬁrst element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; The addend is the second item of the sum list:
(define (addend s) (cadr s))

;; The augend is the third item of the sum list:
(define (augend s) (caddr s))

;; A product is a list whose ﬁrst element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

;; The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))

(check-equal?
 (deriv '(+ x 3) 'x)
 '1
 )

(check-equal?
 (deriv '(* x y) 'x)
 'y)

(deriv '(* (* x y) (+ x 3)) 'x)

;;  Show how to extend the basic diﬀerentiator to handle more kinds of expressions. For instance, implement the diﬀerentiation rule:
;; $ \frac{d(u^n)}{dx} = nu^{n - 1}(\frac{du}{dx}) $ 
;; by adding a new clause to the deriv program and deﬁning appropriate procedures exponentiation?, base, 
;; exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in 
;; the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself. 

(define (exponentiation? exponentiation)
  (and (pair? exponentiation) (eq? (car exponentiation) '**))
  )

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (base exponentation) (cadr exponentation))
(define (exponent exponentation) (caddr exponentation))

(check-true (exponentiation? '(** x 2)))
(check-false (exponentiation? '(+ x 2)))

(check-equal? (make-exponentiation 2 8) 256)
(check-equal? (make-exponentiation 'x 0) 1)
(check-equal? (make-exponentiation 'x 1) 'x)
(check-equal? (make-exponentiation 1 'x) 1)
(check-equal? (make-exponentiation 'x 3) '(** x 3))

(check-equal? (base '(** 2 4)) 2)
(check-equal? (exponent '(** 2 4)) 4)

(check-equal? (make-subtract 4 2) 2)
(check-equal? (make-subtract 'x 2) '(- x 2))
(check-equal? (make-subtract 'x 0) 'x)
(check-equal? (make-subtract 0 'x) '(- 0 x))

(check-equal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))

