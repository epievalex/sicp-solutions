#lang sicp
(define (f n k)
  (cond ((or (= k 0) (= k n)) 1)
        ((or (= k (- 1)) (= n (- 1))) 0)
        (else (+ (f (- n 1) (- k 1)) (f (- n 1) k)))))
(f 10 3)