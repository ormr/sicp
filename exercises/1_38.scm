#lang sicp

(define (d n)
  (cond ((= n 0) 1)
        ((= n 1) 2)
        ((and (= (d (- n 1)) 1)
              (= (d (- n 2)) 1)) (+ (d (- n 1)) (d (- n 2)) (d (- n 3))))
         (else 1)))

(define (<= x y) (or (< x y) (= x y)))
(define (>= x y) (or (> x y) (= x y)))

(define (cond-frac n-frac d-frac k)
  (define (cond-frac-iter n d i store)
    (let ((i-next (if (= i 0) 1 (- i 1))))
      (cond ((= i k) (cond-frac-iter n d i-next (+ (d i-next) (/ (n k) (d k)))))
          ((and (< i k) (> i 0)) (cond-frac-iter n d i-next (+ (d i-next) (/ (n i) store))))
          ((<= i 0) (/ (n i) store)))))
  (cond-frac-iter n-frac d-frac k 0))


(+ (cond-frac (lambda (i) 1.0) d 20) 2)
