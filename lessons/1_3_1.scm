#lang sicp

(define (cube x) (* x x x))

; (define (sum-integers a b)
;   (if (> a b) 0 (+ a (sum-integers (+ a 1) b))))

; (define (sum-cubes a b)
;   (if (> a b) 0 (+ (cube a) (sum-cubes (+ a 1) b))))

; (define (pi-sum a b)
;   (if (> a b) 0 (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; Вытягиваем скрытый шаблон и создаем на его основе процедуру создавая таким образом абстракцию суммы

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; Использование процедуры высшего порядка sum
; Теперь мы можем использовать абстракцию (идею) сложения для построения других абстракций

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))


(sum-integers 1 10)
(sum-cubes 1 10)
(* 8 (pi-sum 1 1000000))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
