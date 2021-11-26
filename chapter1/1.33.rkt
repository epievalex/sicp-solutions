#lang racket
(require "1.28.rkt")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (iter-filtered-accumulate filter combiner null-value term a next b)
  (define (filtered-accumulate a result)
    (if (> a b)
        result
        (if (filter a)
            (filtered-accumulate (next a) (combiner (term a) result))
            (filtered-accumulate (next a) result))))
  (filtered-accumulate a null-value))

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                (filtered-accumulate filter combiner null-value term (next a) next b))
          (combiner 0 (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (prime-squares-sum term a next b)
  (define (addition a b)
    (+ a b))
  (iter-filtered-accumulate prime? addition 0 term a next b))

(define (product-of-relative-primes-n term a next b)
  (define (is-prime-relatively-n? a)
    (= (gcd b a) 1))
  (define (multiplication a b)
    (* a b))
  (iter-filtered-accumulate is-prime-relatively-n? multiplication 1 term a next b))

(prime-squares-sum square 0 inc 10)
(product-of-relative-primes-n identity 0 inc 10)