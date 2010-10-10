;;;
;;; Project Euler - Problem 21
;;;

#|
Let d(n) be defined as the sum of proper divisors of n (numbers less
than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable
pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are
1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
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

(define d
  (lambda (n)
    (apply + (drop-right (divisors n) 1))))

(define amicable-number?
  (lambda (a)
    (let ((b (d a)))
      (and (= (d b) a) (not (= a b))))))
         
(apply + (filter amicable-number? (iota 10000 1)))
;;=> 31626

;;----------------------------------------------------------------------

;;
;; cartesian-product を使用しない divisors
;;
(define (dona a b)
  ;; (dona '(1 5) '(1 3)) => (1 5 3 15)
  (define (scale k l)
    (map (lambda (x) (* k x)) l))
  (append-map (lambda (x) (scale x a)) b))

(define (iter a b)
  (cond [(null? a) '()]
        [(null? b) a]
        [else (iter (dona a (car b)) (cdr b))]))

;(iter '(1 5) '((1 3) (1 2 4)))

(define (divisors n)
  (let ((F (bar (exponential-form (factors n)))))
    (sort (iter (car F) (cdr F)))))
