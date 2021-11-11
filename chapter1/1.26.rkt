#lang sicp
(define times-to-test 1000)

(define (square a)
  (* a a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (random 10000)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? a)
  (fast-prime? a times-to-test))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display (/ elapsed-time 1000.0))
  (display "ms"))

(define (start-prime-test n start-time count)
  (cond
    ((> count 3)
     (newline))
    
    ((prime? n)
         (report-prime n (- (runtime) start-time))
         (start-prime-test (+ n 1) (runtime) (+ 1 count)))
    (else (start-prime-test (+ n 1) (runtime) count))))

(define (timed-primed-test n)
  (start-prime-test n (runtime) 1))

(define (search-for-primes start)
  (timed-primed-test start))

(search-for-primes 1000)
(search-for-primes 1000000)
;In the original case we have "b^n = (b^(n/2))^2" approach, what gives us O(log(n)) performance,
;but here we use b^n = b^(n/2) * b^(n/2) way, and if we decompose equation with definite n, we'll
;get b^n = b1*b2*b3*b4*b5*b6*b7*b8...bn, which essentially has O(n) performance. For example,
;b^16 = b^8 * b^8 = b^4 * b^4 * b^4 * b^4 = b^2 * b^2 * b^2 * b^2 * b^2 * b^2 * b^2 * b^2 = b1*b2*b3*b4*b5*b6*b7*b8...bn.


