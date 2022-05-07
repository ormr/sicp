#lang sicp

; Термы
(define (identity x) x)
(define (square x) (* x x))

; Проверка на простоту

(define (even? n) (= (remainder n 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (even? test-divisor)
    (+ test-divisor 1)
    (+ test-divisor 2)))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

; Комбинаторы
; Сумма
(define (add a b) (+ a b))
; Произведение
(define (mult a b) (* a b))

; Абстракция накопления с фильтром
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (apply-filter x) (if (filter a) x null-value))
    (if (> a b)
      null-value
      (combiner
        (apply-filter (term a))
        (filtered-accumulate
          combiner null-value term (next a) next b filter))))


; Процедура получения следующего значения 
; (define (inc x) (+ x 1))

; Накопление cуммы с применением процедуры высшего порядка
(define (sum term a next b filter)
  (filtered-accumulate add 0 term a next b filter))

; Накопление произведения с применением процедуры высшего порядка
(define (product term a next b filter)
  (filtered-accumulate mult 1 term a next b filter))

; Сумма чисел в диапазоне
(define (sum-integers a b filter)
  (sum identity a inc b filter))

; Произведение чисел в диапазоне
(define (product-integers a b filter)
  (product identity a inc b filter))

(sum-integers 1 4 prime?)
(product-integers 1 10 prime?)
