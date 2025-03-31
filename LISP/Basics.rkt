#lang racket


(define (sum a b)  (+ a b))
;;  +(a b)
(sum 1 2)
(define (fsum f a b)
  (+ (f a) (f b)))

(define (sq x) (* x x))
(fsum sq 3 4)

(define (inc_k k)
  (define (inner_func x) (sum x k))
  inner_func)

(define (inc_5 x) ((inc_k 5) x))

(inc_5 5)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

(define (diff_max_min a b)
  (define max_ab (max a b))
  (define min_ab (min a b))
    (- max_ab min_ab))

(diff_max_min 3 5)
  




