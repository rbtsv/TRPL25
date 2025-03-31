#lang racket

(define (_cons x y)
  (define (_despatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))
    )
  )
_despatch)

(define (_car z) (z 0))
(define (_cdr z) (z 1))

(define p (_cons 1 2))
(_car p)
(_cdr p)