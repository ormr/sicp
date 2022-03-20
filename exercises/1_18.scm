#lang racket

(define (even? n) (= (remainder n 2) 0))
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b n)
  (cond ((= b 0) n)
         ((even? b) (fast-mult-iter (double a) (halve b) n))
         (else (fast-mult-iter a (- b 1) (+ a n)))))

(fast-mult 4 10)
