#lang sicp

; Первое представление

(define (rectangle-perimeter rectangle) 
  (* 2 (+ (rectangle-width rectangle) 
          (rectangle-height rectangle))))

(define (rectangle-area rectangle)
  (* (rectangle-width rectangle) (rectangle-height rectangle)))

(define (make-rectangle topleft bottomright)
  (cons topleft bottomright))

(define (rectangle-topleft rectangle)
  (car rectangle))

(define (rectangle-bottomright rectangle)
  (cdr rectangle))

(define (rectangle-width rectangle)
  (abs (- (x-point (rectangle-topleft rectangle))
          (x-point (rectangle-bottomright rectangle)))))

(define (rectangle-height rectangle)
  (abs (- (x-point (rectangle-topleft rectangle))
          (x-point (rectangle-bottomright rectangle)))))

(define (rectangle-height rectangle)
  (abs (- (y-point (rectangle-topleft rectangle))
          (y-point (rectangle-bottomright rectangle)))))

; Второе представление

(define (make-rectangle topleft width height)
  (cons topleft bottomright))

(define (rectangle-topleft rectangle)
  (car rectangle))

(define (rectangle-width rectangle)
  (car (cdr rectangle)))

(define (rectangle-height rectangle)
  (cdr (cdr rectangle)))
