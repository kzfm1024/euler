;;;
;;; Project Euler - Problem 6
;;;

#|
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten
natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.
|#

(use srfi-1)

(define (square x) (* x x))

(- (square (fold + 0 (iota 100 1)))
   (fold + 0 (map square (iota 100 1))))
;=> 25164150
