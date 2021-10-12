#lang racket/base
(require racket/trace)

(define (even? n) (= (remainder n 2) 0))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mult a b count)
  (cond ((= b 0) count)
        ((even? b) (fast-mult (double a) (halve b) count))
        (else (fast-mult a (- b 1) (+ count a)))))

(trace fast-mult)

(fast-mult 17 19 0)
