#lang sicp

; expmod

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (pseudo-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

; fast-prime

(define (fermat-test n) 
  (define (try-it a) 
    (= (pseudo-expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

(fast-prime? 1000037 100)

(define (timed-prime-test n) 
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
  (if (fast-prime? n 10000) 
      (report-prime n (- (runtime) start-time)) 
      false))

(define (report-prime n elapsed-time) 
  (newline) 
  (display n) 
  (display " *** ") 
  (display elapsed-time) 
  (newline) 
  true)

(define (search-for-primes a b)
  (cond ((and (even? a) (or (< a b) (= a b))) (search-for-primes (+ a 1) b))
        ((and (not (even? a)) (or (< a b) (= a b)))
         (timed-prime-test a)
         (search-for-primes (+ a 2) b))))

; test

(define (test-prime-alghorithm)
  (newline)
  (display " --- 1000 --- ")
  (search-for-primes 1009 1019)
  (newline)
  (display " --- 10.000 --- ")
  (search-for-primes 10007 10039))
  ; (newline)
  ; (display " --- 100.000 --- ")
  ; (search-for-primes 100003 100043)
  ; (newline)
  ; (display " --- 1.000.000 --- ")
  ; (search-for-primes 1000003 1000037)
  ; (newline)
  ; (display " --- 10.000.000 --- ")
  ; (search-for-primes 10000100 10000140))

; (test-prime-alghorithm)
; (fast-prime? 5 100)


