#lang racket

(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))


(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) (* b n)))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(fast-expt 5 5)
