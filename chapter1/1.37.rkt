#lang sicp
;φ = 1/Φ = 0.61803

(define (cont-frac n d k)
  (cond
    ((< k 0) 0)
    (else (/ (n (- k (- k 1)))
             (+ (d (- k 1))
                (cont-frac n d (- k 1)))))))


(define (cont-frac-iter n d k result)
  (cond
    ((< k 0) result)
    (else (cont-frac-iter
           n
           d
           (- k 1)
           (/ (n k) (+ (d k) result))))))

;For k = 10 the procedures gives the correct approximation
;with precision up to 4 digits after the decimal point