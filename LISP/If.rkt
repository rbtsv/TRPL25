#lang racket

(define (Fib n)
  (if (<= n 2) 1
      (+
       (Fib (- n 1))
       (Fib (- n 2))
      )
  )
)


(Fib 5)
(Fib 6)

(define (_abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ;((< x 0) (- x))))
        (else (- x))))
(_abs -7)

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))
      ))

(gcd 121 33)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)
  )
)

(new-if (< 5 0) 1 2)

(define (new-gcd a b)
  (new-if (= b 0) a
      (new-gcd b (remainder a b))
      ))
(new-gcd 121 33)