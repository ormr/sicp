#lang racket

(define (factorial n)
  (if (= n 1)
    n
    (* n (factorial (- n 1)))))

(define (binominal-coeff n k) 
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;(/ (factorial 2) (* (factorial 1) (factorial (- 2 1))))

(binominal-coeff 5 3)
