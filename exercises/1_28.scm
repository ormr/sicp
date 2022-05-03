#lang sicp

(define (square n) (* n n))

; Добавить сигнал для обнаружения нетривиального квадратного корня из 1
; понять как работает expmod

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define a (+ 1 (random (- n 1))))
  (= a (expmod a n n)))

; (miller-rabin-test 70)

; (define (miller-rabin-test n)
;   (define a (+ 1 (random (- n 1)))))

(expmod 3 4 4)
