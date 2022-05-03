#lang sicp

(define (square n) (* n n))

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (fermat-test n a) 
  (= (expmod a n n) a))

(define (find-carmichael-number-iter n a)
  (cond ((or (= n a) (< n a))
         (display n)
         (display " is prime number")
         (newline))
        ((fermat-test n a) (find-carmichael-number-iter n (+ a 1)))
        ((not (fermat-test n a)) a)))

(define (find-carmichael-number n)
  (find-carmichael-number-iter n 1))

(find-carmichael-number 561)
(find-carmichael-number 1105)
(find-carmichael-number 1729)
(find-carmichael-number 2465)
(find-carmichael-number 2821)
(find-carmichael-number 6601)
