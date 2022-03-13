#lang racket

;(define (square x) (* x x))

;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))))

;(define (even? n) (= (remainder n 2) 0))

;(fast-expt 4 4)


(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

(define (fast-expt-iter b n a)
  (cond ((= a 0) a)
        ((even? n) (fast-expt-iter b n (square ())))
        (else (fast-expt-iter b n (square (* b a))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;(fast-expt 4 0)
