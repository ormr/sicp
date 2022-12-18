#lang sicp

; Определяем больше равно и меньше равно для упрощения кода
(define (<= x y) (or (< x y) (= x y)))
(define (>= x y) (or (> x y) (= x y)))

; Определяем cond-frac как обертку над calculate
(define (cond-frac n d k)
  (define (calculate n-frac d-frac i)
    (cond ((<= i 0) (/ (n-frac (+ i 1)) (calculate n-frac d-frac (+ i 1))))
          ((and (>= i 1) (<= i k))
           (+ (d-frac i) (/ (n-frac (+ i 1)) (calculate n-frac d-frac (+ i 1)))))
          (<= i k) (+ d-frac i) (/ (n-frac k) (d-frac k))))
  (calculate n d 0))


(cond-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
  20)

; 30 -> 0.6180339887496662
; 28 -> 0.6180339887483358
; 27 -> 0.618033988753964

