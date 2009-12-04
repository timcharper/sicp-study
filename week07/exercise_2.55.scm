(let ((v ''abracadabra))
  (display "v - ")
  (display v) (newline)
  (display "(car v) - ")
  (display (car v)) (newline)
  (display "(cdr v) - ")
  (display (cdr v)) (newline)
  (display "(car (cdr v)) - ")
  (display (car (cdr v))) (newline)
  (display "(cdr (cdr v)) - ")
  (display (cdr (cdr v))) (newline)
  )

;; for some reason it creates a list with (quote abracadabra)


