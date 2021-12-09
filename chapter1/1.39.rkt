#lang sicp

(define (square x)
  (* x x))

(define (cont-frac-iter n d k result)
  (cond
    ((= k 0) result)
    (else (cont-frac-iter
           n
           d
           (- k 1)
           (/ (n k) (- (d k) result))))))

(define (tan-cf x k)
  (cont-frac-iter
   (lambda (i)
     (if (= i 1)
         x
         (square x)))
   (lambda (i)
     (- (* i 2) 1))
   10
   0.0))

;(tan-cf 1 10) ;1.557407724654902