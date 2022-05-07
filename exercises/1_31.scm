#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (identity x) x)

; Рекурсивный процесс
; (define (product term a next b)
;   (if (> a b)
;     1
;     (* (term a)
;        (product term (next a) next b))))

; Итеративный процесс
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-cubes a b)
  (product cube a inc b))

(define (product-integers a b)
  (product identity a inc b))

(product-integers 1 6)
(product-cubes 1 6)
