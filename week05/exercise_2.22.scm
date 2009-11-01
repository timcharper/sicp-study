;; Louis is taking the arguments off the front of one list, and pushing them on the end of the newly constructed one.  This is simil

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; since this list is being formed recursively, he's basically building up:
;; (square-list '(1 2 3 4))
;; (iter '(1 2 3 4) nil)
;; (iter '(2 3 4) '(1))
;; (iter '(3 4) '(4 1))
;; (iter '(4) '(9 4 1))
;; (iter '() '(16 9 4 1))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; (square-list '(1 2 3 4))
;; (iter '(1 2 3 4) '())
;; (iter '(2 3 4) (cons '() 4))
