;;; 
;;; Project Euler - Problem 16
;;; 

#|
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
|#

(apply + (map
          (lambda (c) (digit->integer c))
          (string->list (number->string (expt 2 1000)))))
;=> 1366
