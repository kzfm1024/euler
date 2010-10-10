;;;
;;; Project Euler - Problem 1
;;;

#|
If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#

(use srfi-1)

(define (multiples-of? m)
  (lambda (n) (zero? (remainder n m))))

(define (multiples-of-3-or-5? n)
  (or ((multiples-of? 3) n)
      ((multiples-of? 5) n)))

(fold + 0 (filter multiples-of-3-or-5? (iota 999 1)))
;=> 233168

(fold + 0 (filter multiples-of-3-or-5? (iota 9 1)))
;=> 23
