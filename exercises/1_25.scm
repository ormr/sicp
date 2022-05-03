#lang sicp

(define (square n) (* n n))

(define (even? n) (= (remainder n 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (even? test-divisor)
    (+ test-divisor 1)
    (+ test-divisor 2)))

; --- fast-prime

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; --- timed prime test

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-fast-prime-test n (runtime)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 3)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((and (even? a) (or (< a b) (= a b))) (search-for-primes (+ a 1) b))
        ((and (not (even? a)) (or (< a b) (= a b)))
         (timed-prime-test a)
         (search-for-primes (+ a 2) b))))

; (define (test-prime-alghorithm)
;   (newline)
;   (display " --- 1000 --- ")
;   (search-for-primes 1009 1019)
;   (newline)
;   (display " --- 10.000 --- ")
;   (search-for-primes 10007 10039)
;   (newline)
;   (display " --- 100.000 --- ")
;   (search-for-primes 100003 100043)
;   (newline)
;   (display " --- 1.000.000 --- ")
;   (search-for-primes 1000003 1000037)
;   (newline)
;   (display " --- 10.000.000 --- ")
;   (search-for-primes 10000100 10000140)
;   (display " --- 10.000.000.000.000 --- ")
;   (search-for-primes 10000000000000 10000000000100))

; (test-prime-alghorithm)
