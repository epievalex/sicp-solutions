#lang racket
(define (square x) (* x x))

(define (f g)
  (g 2))

(f square) ;4
(f (lambda (z) (* z (+ z 1)))) ;6
;(f f) throws an error because of (f 2) doesn't not take a procedure as an argument
