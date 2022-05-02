#lang sicp
; TODO
; Написать процедуру search-for-primes, которая проверяет на простоту все нечетные числа в заданном диапазоне

(define (square n) (* n n))

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (define (divides? a b)
      (= (remainder b a) 0))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? n) (= (remainder n 2) 0))

(define (search-for-primes a b)
  (cond ((> a b) (timed-prime-test a))
        ((even? a) (search-for-primes (+ a 1) b))
        (else 
          (timed-prime-test a)
          (search-for-primes (+ a 1) b))))

(search-for-primes 1000000 1000100)
