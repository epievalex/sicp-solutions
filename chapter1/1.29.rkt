#lang sicp

(define (inc x) (+ x 1))

(define (even? x) (= (remainder x 2) 0))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-intergration f a b n)
  (define h (/ (- b a) n))

  (define (yk idx)
    (+ a (* idx h)))

  (define (term idx)
    (* (cond
         ((= idx 0) 1)
         ((= idx n) 1)
         ((even? idx) 2)
         (else 4))
       (f (yk idx))))

  (* (/ h 3)
     (sum term 0 inc n)))

(simpsons-intergration cube 0 1.0 100)
(simpsons-intergration cube 0 1.0 1000)

;Result
;1st formula gives 0.24998750000000042
;Simpson's integration for n=100 gives 0.24999999999999992
;                      for n=1000 gives 0.2500000000000003
;Above we can see that Simpson's approach's more accurate.