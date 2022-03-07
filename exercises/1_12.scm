#lang racket

(define (print . vs)
  (for-each display vs))

(define (factorial n)
  (if (or (= n 1) (= n 0)) 1
    (* n (factorial (- n 1)))))


(define (pascal-triangle row)
  (define (binomial-coefficient n k)
    (/ (factorial n) (* (factorial k) (factorial (- n k)))))

  (define (show-row row)
    (define (show-row-iter row count)
      (if (= count row)
          (print (binomial-coefficient row count))
          (begin
            (print (binomial-coefficient row count))
            (show-row-iter row (+ count 1)))))
    (show-row-iter row 0))

  (define (pascal-triangle-iter row count)
    (if (= row count)
      (begin
        (show-row count))
      (begin
        (show-row count)
        (newline)
        (pascal-triangle-iter row (+ count 1)))))

  (pascal-triangle-iter row 0))

(pascal-triangle 50)
