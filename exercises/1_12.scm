#lang racket

(define (print . vs)
  (for-each display vs))

(define (factorial n)
  (if (or (= n 1) (= n 0)) 1
    (* n (factorial (- n 1)))))

(define (binomial-coefficient n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(define (show-row row count)
  (if (= row count)
      1
      (print (binomial-coefficient row (+ count 1)))))

(show-row 5 0)

;(define (pascal-triangle row count)
;  (if (= row 5)
;      row
;      (binomial-coefficient row count))


;(binomial-coefficient 0 0)
;(binomial-coefficient 5 0)
;(binomial-coefficient 5 1)
;(binomial-coefficient 5 2)
;(binomial-coefficient 5 3)
;(binomial-coefficient 5 4)
;(binomial-coefficient 5 5)
