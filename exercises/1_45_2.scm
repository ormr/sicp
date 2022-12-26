#lang sicp

(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (power x n)
  (if (= n 1)
    x
    (* x (power x (- n 1)))))

(define (log2 x) 
  (/ (log x) (log 2)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (nth-root n x damp-count) 
  (define (f y) (/ x (power y (- n 1)))) 
  (fixed-point ((repeated average-damp damp-count) f) 1.0))

(define (nth-root-empirical n x) 
  (define (f y) (/ x (power y (- n 1)))) 
  (define damp-count (floor (log2 n))) 
  (fixed-point ((repeated average-damp damp-count) f) 1.0))

(nth-root 2 2 1)
(nth-root 16 2 4) 