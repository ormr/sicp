#lang racket

(define (square x) (* x x))

(define (sqrt x)
  (define (good-enough? guess prev-guess)
    (< (abs (- prev-guess guess)) 0.000000001))
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess x)
      (average guess (/ x guess)))
  (define (sqrt-iter guess prev-guess x)
      (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess x) guess x)))
  (sqrt-iter 1.0 0.9 x))
