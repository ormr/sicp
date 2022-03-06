#lang racket

(define (>= x y) (or (> x y) (= x y)))

(define (f n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

;(define (f n) (f-iter n 1))

;(define (f-iter n count)
;  (if (or (< n 3) (= count 3)) n (f-iter (- n count) (+ count 1))))

;(define (f-iter n count)
;  (cond ((< n 3) n)
;        ((>= n 3) n)))

(f 4)
