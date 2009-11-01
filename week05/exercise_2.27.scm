#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (deep-reverse l)
  (map (lambda (l)
         (if (pair? l)
             (deep-reverse l)
             l
             )
         )
       (reverse l)
       )
  )

(check-equal?
 (deep-reverse x)
 (list (list 4 3) (list 2 1))
 )
