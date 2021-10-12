#lang racket/base
(require racket/trace)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square a)
  (* a a))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(smallest-divisor 199)   ;answer is 199
(smallest-divisor 1999)  ;answer is 1999
(smallest-divisor 19999) ;answer is 7
