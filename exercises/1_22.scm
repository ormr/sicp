(define (square n) (* n n))

(define (even? n) (= (remainder n 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))

; --- is prime ---

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n) (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

; --- timed prime test

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
  (cond ((and (even? a) (< a b)) (search-for-primes (+ a 1) b))
        ((and (not (even? a)) (< a b))
         (timed-prime-test a)
         (search-for-primes (+ a 2) b))))

(define (test-prime-alghorithm)
  ; (newline)
  ; (display " --- 1000 --- ")
  ; (search-for-primes 1009 1019)
  ; (newline)
  ; (display " --- 10.000 --- ")
  ; (search-for-primes 10007 10037)
  ; (newline)
  ; (display " --- 100.000 --- ")
  ; (search-for-primes 100003 100043)
  ; (newline)
  ; (display " --- 1.000.000 --- ")
  ; (search-for-primes 1000003 1000037)
  ; (newline)
  ; (display " --- 10.000.000 --- ")
  ; (search-for-primes 10000000 10000140))
  ; (newline)
  ; (display " --- 10.000.000.000 --- ")
  ; (search-for-primes 10000000000 10000000100)
  ; (newline)
  ; (display " --- 10.000.000.000.000 --- ")
  ; (search-for-primes 10000000000000 10000000000100)
  (newline)
  (display " --- 10.000.000.000.000.0 --- ")
  (search-for-primes 100000000000000 100000000000100))


(test-prime-alghorithm)
