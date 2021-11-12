#lang racket

(define (even? x)
  (= (remainder x 2) 0))

(define (inc x)
  (+ x 1))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (simpson-integration f a b n)
  (define h (/ (- b a) n))
  (define (term k)
  (define y (+ a (* k h)))
    (* (f y)(cond
      ((or (= k 0) (= k n)) 1)
      ((even? k) 2)
      (else 4))))
  (* (sum term 0 inc n) (/ h 3)))
  
(integral cube 0 1 0.01)
(simpson-integration cube 0 1.0 100)
;Result
;1st formula gives 0.24998750000000042
;Simpson's integration for n=100 gives 0.24999999999999992
;                      for n=1000 gives 0.2500000000000003
;Above we can see that Simpson's approach's more accurate.