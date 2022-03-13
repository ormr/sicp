#lang racket

; линейно-рекурсивный процесс

(define (expt b n)
  (if (= n 1)
    b
    (* b (expt b (- n 1)))))

; линейно-итеративный процесс

(define (expt2 b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
      product
      (expt-iter b
                  (- counter 1)
                  (* b product))))
  (expt-iter b n 1))

; линейно рекурсивный процесс с порядком роста (O(log(n)))

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

