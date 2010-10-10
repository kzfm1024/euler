;;
;; Project Euler - Problem 5
;;

#|
2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers
from 1 to 20?
|#

(use srfi-1)

(apply lcm (iota 20 1))
;=> 232792560