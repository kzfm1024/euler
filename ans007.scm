;;;
;;; Project Euler - Problem 7
;;;

#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10001st prime number?
|#

(define (prime? n)
  (define (smallest-divisor d)
    (cond [(> (* d d) n) n]
          [(= (remainder n d) 0) d]
          [else (smallest-divisor (+ d 1))]))
  (= n (smallest-divisor 2)))

(define (nth-prime nth)
  (let iter ((i 1) (p 2))
    (cond [(not (prime? p)) (iter i (+ p 1))]
          [(= i nth) p]
          [else (iter (+ i 1) (+ p 1))])))

(nth-prime 10001)
;=> 104743
