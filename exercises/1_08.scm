#lang racket

(define (square x) (* x x))

(define CONST 0.001)

(define (cbrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
    guess
    (cbrt-iter (improve guess x) guess x)))

(define (improve y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough? guess prev)
  (< (abs (- prev guess)) CONST))

(define (cbrt x)
  (cbrt-iter 1.0 0.9 x))

(cbrt 3)
(cbrt 4)
(cbrt 256)
