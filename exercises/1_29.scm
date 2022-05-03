#lang sicp

(define (even? x)
  (= (remainder x 2) 0))
(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (next y) (* 2 y))
  (define (y k) (* f (+ a (* k h))))
  (define (g k)
    (define (c k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            ((even? k) 2)
            (else 4)))
    (* (c k) (f (+ a (* k h)))))
  (/ (* (sum g 0 inc n) h) 3))

  ; (* (/ h 3) (sum inc y a next n)))

(simpsons-rule inc 0 1 100)

