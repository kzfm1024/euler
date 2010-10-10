;;;
;;; Project Euler - Problem 3
;;;

#|
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
|#

(define (factors num)
  (unless (> num 1) (error "must be greater than 1, but got:" num))
  (let iter ((n num) (d 2) (factors '()))
    (cond [(= n 1) factors]
          [(> (* d d) n) (cons n factors)]
          [(zero? (remainder n d)) (iter (/ n d) d (cons d factors))]
          [else (iter n (+ d 1) factors)])))

(car (factors 600851475143))
;=> (car '(6857 1471 839 71)) => 6857

;----------------------------------------------------------------------

(use util.stream)

(define (square x) (* x x))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
 (stream-cons (stream-car stream)
              (sieve (stream-filter
                      (lambda (x)
                        (not (divisible? x (stream-car stream))))
                      (stream-cdr stream)))))

(define *primes* (sieve (stream-iota -1 2)))

(define (prime-factors n)
  (define (iter n primes factors)
    (let ((d (stream-car primes)))
      (cond [(> (square d) n) (cons n factors)]
            [(divisible? n d) (iter (/ n d) primes (cons d factors))]
            [else (iter n (stream-cdr primes) factors)])))
  (iter n *primes* '()))

(prime-factors 13195)
;=> (29 13 7 5)

(prime-factors 600851475143)
;=> (6857 1471 839 71)

