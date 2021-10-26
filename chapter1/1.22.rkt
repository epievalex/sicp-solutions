#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square a)
  (* a a))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? a)
  (= a (smallest-divisor a)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (display "ms "))

(define (start-prime-test n start-time count)
  (cond
    ((> count 3)
     (newline)
     (display "search-finished"))
    
    ((and (prime? n))
         (report-prime n (- (runtime) start-time))
         (start-prime-test (+ n 1) (runtime) (+ 1 count)))
    (else (start-prime-test (+ n 1) (runtime) count))))

(define (timed-primed-test n)
  (start-prime-test n (runtime) 1))

(define (search-for-primes start)
  (timed-primed-test start))

(search-for-primes 1000)
;1009 *** 2ms 
;1013 *** 2ms 
;1019 *** 1ms
(newline)
(search-for-primes 10000)
;10007 *** 4ms 
;10009 *** 5ms 
;10037 *** 4ms
(newline)
(search-for-primes 100000)
;100003 *** 13ms 
;100019 *** 12ms 
;100043 *** 13ms 
(newline)
(search-for-primes 1000000)
;1000003 *** 42ms 
;1000033 *** 43ms 
;1000037 *** 44ms