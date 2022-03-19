#lang racket
(define (>= x y) (or (> x y) (= x y))) 
(define (f n)
  (cond ((< n 3) n)
    	((>= n 3) (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))
