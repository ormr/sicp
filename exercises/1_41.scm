#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 5)
((double inc) 5)

(((double double) inc) 6)
