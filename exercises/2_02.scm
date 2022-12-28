#lang sicp

(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define first-point (make-point 4 5))

(define (make-segment start end)
  (cons start end))

(define (start-segment pos) (car pos))
(define (end-segment pos) (cdr pos))

(define example-segment (make-segment (make-point 1 2) (make-point 10 4)))

(define (midpoint-segment segment)
  (let ((start (start-segment segment)) (end (end-segment segment)))
    (make-point
      (/ (+ (x-point start) (x-point end)) 2.0)
      (/ (+ (y-point start) (y-point end)) 2.0))))

(print-point (midpoint-segment example-segment))
