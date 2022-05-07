#lang sicp

; Комбинаторы
; Сумма
(define (add a b) (+ a b))

; Произведение
(define (mult a b) (* a b))

; Абстракция накопления
; Рекурсивный процесс
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; Итеративный процесс
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Применение

(define (identity x) x)

(define (inc x) (+ x 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (accumulate add 0 term a next b))

(define (product term a next b)
  (accumulate-iter mult 1 term a next b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (product-integers a b)
  (product identity a inc b))

(sum-integers 1 10)
(product-integers 1 10)
