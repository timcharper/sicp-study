#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (count-change amount currency)
  (cc amount (- (length currency) 1) currency))

(define (cc amount kinds-of-coins currency)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins -1)) 0)
        (else (+
               (cc amount (- kinds-of-coins 1) currency)
               (cc (- amount (list-ref currency kinds-of-coins)) kinds-of-coins currency)))))


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(check-equal? (count-change 100 us-coins) 292)
(check-equal? (count-change 100 uk-coins) 104561)

