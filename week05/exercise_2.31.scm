#lang scheme/base

(require (planet schematics/schemeunit:3))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)) 2)

(define (map-tree f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree)
  (map-tree (lambda (x) (* x x))
            tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))
 )
