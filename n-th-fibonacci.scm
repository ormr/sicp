(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 1)
    b
    (fib-iter (+ a b) a (- count 1))))

(fib 1)
(fib 2)
(fib 4)
