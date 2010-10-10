;;;
;;; Project Euler - Problem 10
;;;

#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(use srfi-1)

(define (prime? n)
  (define (smallest-divisor d)
    (cond [(> (* d d) n) n]
          [(= (remainder n d) 0) d]
          [else (smallest-divisor (+ d 1))]))
  (= n (smallest-divisor 2)))

(fold + 0 (filter prime? (iota (- 2000000 2) 2)))
;=> 142913828922
