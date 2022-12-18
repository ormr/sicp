#lang sicp

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      mid-point
      (let ((test-value (f mid-point)))
        (cond ((positive? test-value)
               (search f neg-point mid-point))
              ((negative? test-value)
               (search f mid-point pos-point))
              (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "У аргументов не разные знаки" a b)))))

;; (half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                       1.0
                       2.0)

; Нахождение неподвижных точек функции

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;; (fixed-point cos 1.0)
;; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))