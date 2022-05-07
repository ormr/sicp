#lang sicp

; Рекурсивный процесс
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (sum term (next a) next b))))

; Итеративный процесс
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
