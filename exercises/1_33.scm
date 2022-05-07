#lang sicp

(define (filtered-accumulate combiner null-value term a next b filter)
    (if (> a b)
      null-value
    (combiner
      (term a)
      (filtered-accumulate combiner null-value term (next a) next b filter))))
