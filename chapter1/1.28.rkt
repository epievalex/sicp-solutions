#lang racket
(require racket/trace)

(define times-to-test 1000)

(define (square a)
  (* a a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (define remainder-of-square (remainder (square (expmod base (/ exp 2) m))
                    m))
         (if (and (= remainder-of-square 1)
                  (not (> exp 1))
                  (not (< exp (- m 1)))) 0 remainder-of-square))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n times-to-test))

;(prime? 561)
;(prime? 1105)
;(prime? 1729)
;(prime? 2465)
;(prime? 2821)
;(prime? 6601)
;(prime? 1009)
;(prime? 11)
;(prime? 10037)
;(prime? 100003)
;
;Result
;561 isn't prime based on Miller-Rabin's test
;1105 isn't prime based on Miller-Rabin's test
;1729 isn't prime based on Miller-Rabin's test
;2465 isn't prime based on Miller-Rabin's test
;2821 isn't prime based on Miller-Rabin's test
;6601 isn't prime based on Miller-Rabin's test
;
;Actually prime numbers
;1009 is prime based on Miller-Rabin's test
;11 is prime based on Miller-Rabin's test
;10037 is prime based on Miller-Rabin's test
;100003 is prime based on Miller-Rabin's test
