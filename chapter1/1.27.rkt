#lang racket
(require racket/trace)

(define times-to-test 100000)

(define (square a)
  (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder a b) 0))

(define (next a)
  (if (even? a) 3 (+ a 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (slow-prime? a)
  (if (= a (smallest-divisor a)) (display "is prime based on finding divisors")
      (display "isn't prime based on finding divisors")))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) (display " is prime based on Fermat's test; "))
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else (display " isn't prime based on Fermat's test; "))))

(define (prime? n)
  (display n)
  (fast-prime? n times-to-test)
  (slow-prime? n)
  (newline))

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 1009)
(prime? 11)
(prime? 10037)
(prime? 100003)

;Result
;
;561 is prime based on Fermat's test, isn't prime based on finding divisors
;1105 is prime based on Fermat's test, isn't prime based on finding divisors
;1729 is prime based on Fermat's test, isn't prime based on finding divisors
;2465 is prime based on Fermat's test, isn't prime based on finding divisors
;2821 is prime based on Fermat's test, isn't prime based on finding divisors
;6601 is prime based on Fermat's test, isn't prime based on finding divisors
;
;Actually prime numbers
;1009 is prime based on Fermat's test, is prime based on finding divisors
;11 is prime based on Fermat's test, is prime based on finding divisors
;10037 is prime based on Fermat's test, is prime based on finding divisors
;100003 is prime based on Fermat's test, is prime based on finding divisors
