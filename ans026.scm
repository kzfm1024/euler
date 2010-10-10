;;;
;;; Project Euler - Problem 26
;;;

#|
A unit fraction contains 1 in the numerator. The decimal representation
of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d  1000 for which 1/d contains the longest recurring
cycle in its decimal fraction part.
|#

(use srfi-1)
(use gauche.collection)

(define recurring-cycle
  (lambda (d)
    (let iter [(n 1) (d d) (qs '()) (rs '())]
      (receive (q r) (quotient&remainder n d)
        (cond [(zero? r) 0]
              [(zero? q) (iter (* n 10) d (cons q qs) (cons #f rs))]
              [(memv r (reverse rs)) => (cut length <>)]
              [else (iter (* r 10) d (cons q qs) (cons r rs))])))))

(car
 (find-max
  (map (lambda (d) (cons d (recurring-cycle d))) (iota 999 1))
  :key cdr))
;;=> 983

;;----------------------------------------------------------------------

(let [(d #f) (cycle 0)]
  (for-each
   (lambda (p)
     (when (> (cdr p) cycle) 
       (begin (set! d (car p))
              (set! cycle (cdr p)))))
   (map (lambda (d) (cons d (recurring-cycle d))) (iota 998 2)))
  d)

;;----------------------------------------------------------------------

(define foo
  (lambda (n d qs rs)
    (receive (q r) (quotient&remainder n d)
      (cond [(zero? q) (foo (* n 10) d (cons q qs) (cons #f rs))]
            [(zero? r) (list (reverse! (cons q qs)) (reverse! (cons r rs)))]
            [(memv r rs) (list (reverse! (cons q qs)) (reverse! (cons r rs)))]
            [else (foo (* r 10) d (cons q qs) (cons r rs))]))))

(foo 1 2 '() '())

(foo 1 4 '() '())

(foo 1 8 '() '())

(foo 1 7 '() '())
