#lang racket

(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (even? n) (= (remainder n 2) 0))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 5 5)
