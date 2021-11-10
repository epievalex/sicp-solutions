#lang racket

(define (runtime) (current-milliseconds))

(define (square a)
  (* a a))

(define (fast-expt a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt a (/ n 2))))
        (else (* a (fast-expt a (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? a)
  (fast-prime? a 1000))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
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
(search-for-primes 10000)
(search-for-primes 100000)

;Results
;1009 *** 46ms 
;1013 *** 41ms 
;1019 *** 41ms
;
;10007 *** 2531ms 
;10009 *** 2708ms 
;10037 *** 2538ms
;
;100003 *** 222754ms
;
;I've tried to calculate primes after 100000, but it takes me about 3-4 minutes for only one value, so I stopped a computation.
;I'd also like to note that this approach requires a lot of memory.
;Much slower result is caused by a calculation of the dividend that can be really big to slow down the final result.
