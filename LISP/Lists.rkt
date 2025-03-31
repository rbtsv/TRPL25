#lang racket


(define (sum x y . lst)
  (define (_sum _lst)
    (if (null? _lst)
        0
        (+ (car _lst) (_sum (cdr _lst)))
        )  
  )
 (+ x y (_sum lst)))

(sum 1 2 3 4 5 6 7 8 9 10)

(sum 1 2)

(sum 1)
  
