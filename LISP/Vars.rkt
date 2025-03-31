#lang racket

(define (sum_of_sq x y)
  (define (sq z) (* z z))
  (+ (sq x) (sq y))
)

(sum_of_sq 2 3)

(define (sq z) (* z z))

(define (pm_of_sq x y)
  (let ((sq_x (sq x)) (sq_y (sq y))) 
    (if (> y 0)
        (+ sq_x sq_y)
        (+ sq_x sq_x))))

(pm_of_sq 2 3)