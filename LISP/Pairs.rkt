#lang racket

(define p (cons 1 2))

(car p)
(cdr p)
(cons 0 p) ; (0, (1 , 2))
(cadr (cons 0 p)) ; cadr x = car(cdr x)
(define my_list '(1 2 3 4 5 6 7 8 9 10))
(+ 1 2 3 4 5 6 7 8 9 10)

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))
  )
)

(sum my_list)