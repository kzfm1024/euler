;;;
;;; Project Euler - Problem 14
;;;

#|
The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1) 
contains 10 terms. Although it has not been proved yet (Collatz Problem), 
it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
|#

(use srfi-1)

(define (collatz-sequence n)
  (define (next n)
    (if (even? n)
        (/ n 2)
        (+ (* 3 n) 1)))
  (if (= n 1)
      (cons n '())
      (cons n (collatz-sequence (next n)))))

(let ((maxn 0) (maxlen 0))
  (for-each 
   (lambda (n)
     (let ((len (length (collatz-sequence n))))
       (when (> len maxlen)
         (begin (set! maxn n)
                (set! maxlen len)))))
   (iota 1000000 1))
  maxn)
;=> 837799
