#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder a b) 0))

(define (even? a)
  (= (remainder a 2) 0))

(define (square a)
  (* a a))

(define (next a)
  (if (even? a) 3 (+ a 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;procedure (+ test-divisor 1) of (find-divisor n (+ test-divisor 1)) executes only with 1 step. But with this approach we need to calculare all (square n) for find-divisor

;procedure (next test-divisor) of (find-divisor n (next test-divisor)) calculates even? and sum procedures,
;what takes us one additional step(calculate condition, calcualte independent expression). There we need to calculate (/ (square n) 2) numbers,
;but else expression requires 2 times more calculations than just (+ test-divisor 1).

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
;old result 1009 *** 2ms, new result 1009 *** 1ms 
;old result 1013 *** 2ms, new result 1013 *** 1ms
;old result 1019 *** 1ms, new result 1019 *** 2ms
(newline)
(search-for-primes 10000)
;old result 10007 *** 5ms, new result 10007 *** 5ms
;old result 10009 *** 5ms, new result 10009 *** 4ms 
;old result 10037 *** 4ms, new result 10037 *** 4ms
(newline)
(search-for-primes 100000)
;old result 100003 *** 13ms, new result 100003 *** 15ms
;old result 100019 *** 12ms, new result 100019 *** 14ms 
;old result 100043 *** 13ms, new result 100043 *** 13ms 
(newline)
(search-for-primes 1000000)
;old result 1000003 *** 39ms, new result 1000003 *** 42ms
;old result 1000033 *** 41ms, new result 1000033 *** 41ms 
;old result 1000037 *** 41ms, new result 1000037 *** 44ms