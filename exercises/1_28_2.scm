#lang sicp
; Задание 1.28 (Попытка самостоятельного решения)
; Если n 

(define (trivial-test k)
  (if (and ()))

(define modified-expmods)

; Проверка если k = 1 или k = n
(define (apply-remainder-or-trivial k))

(define (apply-trivial-check k m r)
  (if (and 
        (not (= k 1))
        (not (= k (- m 1)))
        (= r 1))
    0
    r))

(define (remainder-or-trivial k m)
  (apply-remainder-or-trivial k m (remainder (square k) m)))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder-or-trivial (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

; Допилить
; (define (miller-rabin-test n)
;   ()
