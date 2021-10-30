#lang sicp
(define times-to-test 1000000.0)

(define (square a)
  (* a a))

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
  (try-it (+ 90 (random 10))))

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
  (display elapsed-time)
  (display "ms "))

(define (start-prime-test n start-time count)
  (cond
    ((> count 3)
     (newline))
    
    ((prime? n)
         (report-prime n (/ (- (runtime) start-time) times-to-test))
         (start-prime-test (+ n 1) (runtime) (+ 1 count)))
    (else (start-prime-test (+ n 1) (runtime) count))))

(define (timed-primed-test n)
  (start-prime-test n (runtime) 1))

(define (search-for-primes start)
  (timed-primed-test start))

(search-for-primes 1000)
  
(search-for-primes 1000000)

(search-for-primes 1000000000000)

(search-for-primes 1000000000000000000000000)

;According to growth order O(log n), we should to make 2 times more calculations for n^2 then for just n,
;but it doesn't mean that time for calculation itself is considered. In other words 0(log n) growth order
;shows only amount of steps, but not time needed for calculation, for this, most likely there is a definition,
;depending on the type of processor, performance, etc.

;Below is a list of calculations, where each next call has a number that is the square of the previous one.

;1009 *** 1.511189ms 
;1013 *** 1.533599ms 
;1019 *** 1.578705ms

;1000003 *** 2.348156ms 
;1000033 *** 2.394838ms 
;1000037 *** 2.421136ms

;1000000000039 *** 7.88598ms 
;1000000000061 *** 8.272988ms 
;1000000000063 *** 8.165992ms

;1000000000000000000000007 *** 27.001549ms 
;1000000000000000000000049 *** 26.540861ms 
;1000000000000000000000121 *** 28.787124ms