;;;
;;; Project Euler - Problem 20
;;;

#|
n! means n * (n - 1) * ... * 3 * 2 * 1

Find the sum of the digits in the number 100!
|#

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(apply +
       (map digit->integer
            (string->list (number->string (fact 100)))))
;=> 648
