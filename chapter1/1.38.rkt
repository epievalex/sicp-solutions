#lang sicp

(define (cont-frac-iter n d k result)
  (cond
    ((= k 0) result)
    (else (cont-frac-iter
           n
           d
           (- k 1)
           (/ (n k) (+ (d k) result))))))

(define (eulers-formula x k)
  (+
   2
   (cont-frac-iter
   (lambda (i) 1.0)
   (lambda (i)
     (if (= (remainder i 3) 2)
         (* 2 (/ (+ i 1) 3))
         1))
   10
   0.0)))

;(eulers-formula 1 10) ;2.7182817182817183