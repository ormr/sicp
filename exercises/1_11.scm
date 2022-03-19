#lang racket

(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1) (- n 2) (- n 3))))
