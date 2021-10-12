#lang racket/base
(require racket/trace)

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt b n a)
  (cond ((or (= n 0) (= n 1)) a)
        ((even? n) (fast-expt b (/ n 2) (* a (square b))))
        (else (fast-expt b (- n 1) (* b a)))))

(trace fast-expt)

(fast-expt 9 7 1)
