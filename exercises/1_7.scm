#lang racket

(define (square x) (* x x))

(define CONST 0.001)

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess) 2))

(define (average x y n)
  (/ (+ x y) n))

(define (good-enough? guess prev)
  (< (abs (- prev guess)) CONST))

(define (sqrt x)
  (sqrt-iter 1.0 0.9 x))

(sqrt 2)
(sqrt 4)
