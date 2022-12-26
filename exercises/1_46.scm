#lang sicp

(define (square x) (* x x))

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next) guess (try (improve guess)))))
  (lambda (guess) (try guess)))

; fixed-point from 1.3.4

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; (define (sqrt x)
;;   (newtons-method (lambda (y) (- (square y) x))
;;                   1.0))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? next guess)
    (< (abs (- (square next) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 49)
