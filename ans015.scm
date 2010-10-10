;;; 
;;; Project Euler - Problem 15
;;; 

#|
Starting in the top left corner of a 2x2 grid, there are 6 routes (without
backtracking) to the bottom right corner.

How many routes are there through a 20x20 grid?
|#

(use srfi-1)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (pascal-triangle n)
  (define (C n k)
    (/ (fact n)
       (* (fact (- n k))
          (fact k))))
  (map (lambda (k) (C n k)) (iota (+ n 1))))

(define (reduce-adjacent l)
  (cond [(null? l) '()]
        [(null? (cdr l)) '()]
        [else (cons (+ (car l) (cadr l))
                    (reduce-adjacent (cdr l)))]))

(define (number-of-routes l)
  (cond [(null? l) (error "must be non-empty list")]
        [(null? (cdr l)) (car l)]
        [else (number-of-routes (reduce-adjacent l))]))

(number-of-routes (pascal-triangle 20))
; => 137846528820

;;----------------------------------------------------------------------

;; (2n)! / (n! * n!)

(/ (fact (* 2 20)) (* (fact 20) (fact 20)))

;;----------------------------------------------------------------------

(number-of-routes (pascal-triangle 0))
(number-of-routes (pascal-triangle 1))
(number-of-routes (pascal-triangle 2))
(number-of-routes (pascal-triangle 3))
(number-of-routes (pascal-triangle 20))
(pascal-triangle 0)
(pascal-triangle 1)
(pascal-triangle 2)
(pascal-triangle 3)
(pascal-triangle 20)
