;;;
;;; Project Euler - Problem 23
;;;

#|
A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors
of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
number.

A number n is called deficient if the sum of its proper divisors is less
than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers. However, this upper limit
cannot be reduced any further by analysis even though it is known that the
greatest number that cannot be expressed as the sum of two abundant numbers
is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum
of two abundant numbers.
|#

(use srfi-1)
(use util.combinations)

(define factors
  (lambda (n)
    (unless (positive? n) (error "must be positive, but got:" n))
    (if (= n 1)
        '(1)
        (let iter ((n n) (d 2) (factors '()))
          (cond [(= n 1) factors]
                [(> (* d d) n) (cons n factors)]
                [(zero? (remainder n d))
                 (iter (/ n d) d (cons d factors))]
                [else (iter n (+ d 1) factors)])))))

(define exponential-form
  ;; (exponential-form '(5 3 2 2)) => ((5 . 1) (3 . 1) (2 . 2))
  (lambda (factors)
    (let iter ((a (car factors)) (n 1) (f (cdr factors)))
      (cond [(null? f) (list (cons a n))]
            [(eqv? a (car f)) (iter a (+ n 1) (cdr f))]
            [else (cons (cons a n) (iter (car f) 1 (cdr f)))]))))

(define powers-form
  ;; (power-form '((5 . 1) (3 . 1) (2 . 2))) => ((1 5) (1 3) (1 2 4))
  (lambda (expt-form)
    (let ((powers-of
           (lambda (a n)
             (map (lambda (x) (expt a x)) (iota (+ n 1))))))
      (map (lambda (e) (powers-of (car e) (cdr e))) expt-form))))

(define divisors
  (lambda (n)
    (sort
     (map (lambda (x) (apply * x))
          (cartesian-product
           (powers-form
            (exponential-form (factors n))))))))

(define proper-divisors
  (lambda (n)
    (drop-right (divisors n) 1)))

(define sum-of-proper-divisors
  (lambda (n)
    (apply + (proper-divisors n))))

(define abundant-number?
  (lambda (n)
    (> (sum-of-proper-divisors n) n)))

(define delete-same-number
  (lambda (s)
    (cond [(null? s) '()]
          [(null? (cdr s)) s]
          [else (if (= (car s) (cadr s))
                    (delete-same-number (cdr s))
                    (cons (car s) (delete-same-number (cdr s))))])))

(define all-sum-of-two-numbers
  (lambda (s)
    (if (null? s)
        '()
        (append (map (lambda (x) (+ (car s) x)) s)
                (all-sum-of-two-numbers (cdr s))))))

(- (apply + (iota 23123 1))
   (apply + (delete-same-number
             (sort
              (filter (lambda (n) (<= n 23123))
                      (all-sum-of-two-numbers
                       (filter abundant-number? (iota 23123 1))))))))
;;=> 4179871

;;----------------------------------------------------------------------

;; 1..23123 の数列の和から23123 以下の abundant 数の数列の和を引く
(define *the-numbers*
  (delete-same-number
   (sort
    (filter (lambda (n) (<= n 23123))
            (all-sum-of-two-numbers
             (filter abundant-number? (iota 23123 1)))))))

(- (apply + (iota 23123 1)) (apply + *the-numbers*))
