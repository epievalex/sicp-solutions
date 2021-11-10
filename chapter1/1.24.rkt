#lang sicp
(define times-to-test 1000)

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
(search-for-primes 1000000000000)
(search-for-primes 1000000000000000000000000)

;According to growth order O(log n), we should to make 2 times more calculations for n^2 then for just n,
;but it doesn't mean that time for calculation itself is considered. In other words 0(log n) growth order
;shows only amount of steps, but not time needed for calculation, for this, most likely there is a definition,
;depending on the type of processor, performance, etc.
;
;Below is a list of calculations, where each next call has a number that is the square of the previous one.
;
;10007 *** 3.654ms
;10009 *** 3.067ms
;10037 *** 3.742ms
;
;1000003 *** 4.369ms
;1000033 *** 4.844ms
;1000037 *** 4.288ms
;
;1000000000039 *** 20.501ms
;1000000000061 *** 17.14ms
;1000000000063 *** 16.281ms
;
;1000000000000000000000007 *** 54.569ms
;1000000000000000000000049 *** 48.737ms
;1000000000000000000000121 *** 53.985ms