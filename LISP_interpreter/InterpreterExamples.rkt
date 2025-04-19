#lang racket

(define (plus a b) (+ a b))
(plus 3 4)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

(define (Fib n)
  (if (< n 3) 1
  (+ (Fib (- n 1) ) (Fib (- n 2) )
     )))

(Fib 7)

(define (sum_of_sqs x y)
  (define (sq x) (* x x))
  (+ (sq x) (sq y)))

(sum_of_sqs 3 4)
