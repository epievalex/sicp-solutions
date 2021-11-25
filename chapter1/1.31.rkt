#lang racket

(define (square x) (* x x))

(define (product term a next b)
  (define (iter-product a result)
    (if (> a b)
        result
        (iter-product (next a) (* (term a) result))))
  (iter-product a 1))

(define (finding-pi-via-formula n)
  (define (next x)
    (+ x 2))

  (define (term x)
    (if (or (= (- n 1) x) (= x n))
        (/ x (square (- x 1)))
        (square (/ x (- x 1)))))

  (* 8.0 (product term 4 next n)))

(finding-pi-via-formula 250000)

;finding-pi-via-formule procedure takes amount of numbers
;in the divisor and denominator as an argument. The larger
;the exponent n, the more accurate the result of calculating
;the number pi.
;
;The real value of pi number is 3.14159265359
;
;Result
;for n=100 procedure returns 3.157339689217565
;for n=1000 procedure returns 3.143163842419198
;for n=10000 procedure returns 3.1417497371492673
;for n=150000 procedure returns 3.1416031255827583
;for n=250000 procedure returns 3.1415989367813837
  

