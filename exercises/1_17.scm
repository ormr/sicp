#lang racket

(define (fast-mult a b)
  (fast-mult-iter a b 1))

(define (fast-mult-iter a b n)
  (cond ((= n 0) a)
        (else (fast-mult-iter (+ a b) b (- n 1)))))

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

