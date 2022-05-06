#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (identity x) x)

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (product-cubes a b)
  (product cube a inc b))

(define (product-integers a b)
  (product identity a inc b))

(product-integers 1 6)
(product-cubes 1 6)
