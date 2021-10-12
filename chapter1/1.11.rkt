#lang sicp
;recursive proccess
(define (f-rec n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f-rec (- n 1)) (f-rec (- n 2)) (f-rec (- n 3))))))

;iterative proccess
(define (f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (cond ((< count 3) count)
        ((= count 3) (+ a b c))
        (else (f-iter (+ a b c) a b (- count 1)))))


(f-rec -1)
(f -1)
  