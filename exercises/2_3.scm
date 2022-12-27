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

(define (make-segment start end)
  (cons start end))

(define (start-segment pos) (car pos))
(define (end-segment pos) (cdr pos))

(define (midpoint-segment segment)
  (let ((start (start-segment segment)) (end (end-segment segment)))
    (make-point
      (/ (+ (x-point start) (x-point end)) 2.0)
      (/ (+ (y-point start) (y-point end)) 2.0))))


; Rectangle

(define (make-rectangle ab bc)
  (cons ab bc))

(define (ab-side rect) (car rect))
(define (bc-side rect) (cdr rect))

; height = By - Ay
; weight = Cx - Bx

(define (get-height rect)
    (let ((a (start-segment (ab-side rect)))
          (b (end-segment (ab-side rect))))
      (abs (- (y-point b) (y-point a)))))

(define (get-width rect)
    (let ((b (start-segment (bc-side rect)))
          (c (end-segment (bc-side rect))))
      (abs (- (x-point c) (x-point b)))))

(define (get-perimeter rect)
  (let ((a (get-height rect)) (b (get-width rect))) (* (+ a b) 2)))

(define (get-square rect)
  (let ((a (get-height rect)) (b (get-width rect))) (* a b)))

(define ab (make-segment (make-point 2 2) (make-point 2 8)))
(define bc (make-segment (make-point 2 8) (make-point 20 4)))
(define abcd (make-rectangle ab bc))
