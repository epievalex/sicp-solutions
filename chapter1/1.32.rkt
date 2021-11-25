#lang racket

(define (identity x) x)

(define (inc x) (+ x 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (iter-accumulate combiner null-value term a next b)
  (define (accumulate a result)
    (if (> a b)
        result
        (accumulate (next a) (combiner a result))))
  (accumulate a null-value))

(define (sum term a next b)
  (define (addition a b)
    (+ a b))
  (display "The accumulate sum procedure appoarch result is ")
  (accumulate addition 0 term a next b))

(define (iter-sum term a next b)
  (define (addition a b)
    (+ a b))
  (display "The iter-accumulate sum procedure appoarch result is ")
  (iter-accumulate addition 0 term a next b))

(define (product term a next b)
  (define (multiplication a b)
    (* a b))
  (display "The accumulate product procedure appoarch result is ")
  (accumulate multiplication 1 term a next b))

(define (iter-product term a next b)
  (define (multiplication a b)
    (* a b))
  (display "The iter-accumulate product procedure appoarch result is ")
  (iter-accumulate multiplication 1 term a next b))

(sum identity 1 inc 10)
(iter-sum identity 1 inc 10)
(newline)
(product identity 1 inc 10)
(iter-product identity 1 inc 10)

;Result
;The operations of addition and multiplication are performed
;for a series from 1 to 10, i.e. (1 + 2 + ... + 10) and (1 × 2 × ... × 10)
;
;The accumulate sum procedure appoarch result is 55
;The iter-accumulate sum procedure appoarch result is 55
;
;The accumulate product procedure appoarch result is 3628800
;The iter-accumulate product procedure appoarch result is 3628800