#lang sicp

(define (square n) (* n n))

(define (even? n)
   (= (remainder n 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (even? test-divisor) (+ test-divisor 1) (+ test-divisor 2)))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
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

(define (search-for-primes a b)
  (cond ((or (> a b) (= a b)) (timed-prime-test a))
        (else
          (timed-prime-test a)
          (search-for-primes (next a) b))))

(search-for-primes 1 1000000)
