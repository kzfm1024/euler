;;;
;;; Project Euler - Problem 4
;;;

#|
A palindromic number reads the same both ways. The largest palindrome 
made from the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#

(use srfi-1)

(define (palindromic-number? num)
  (let ((l (string->list (number->string num))))
    (equal? l (reverse l))))

(define (products-of-3-digit-numbers)
  (apply append
         (map (lambda (i)
                (map (lambda (j) (* i j)) (iota 900 999 -1)))
              (iota 900 999 -1))))

(apply max (filter palindromic-number? (products-of-3-digit-numbers)))
;=> 906609

;----------------------------------------------------------------------
; ボツ

(define (6-digit-palindrome-numbers)

  (define (foo a b c)
    (+ (* a 100000)
       (* b 10000)
       (* c 1000)
       (* c 100)
       (* b 10)
       (* a 1)))
  
  (define (hoge a b)
    (map (lambda (c) (foo a b c)) '(9 8 7 6 5 4 3 2 1 0)))

  (define (dona a)
    (apply append (map (lambda (b) (hoge a b)) '(9 8 7 6 5 4 3 2 1 0))))

  (apply append (map (lambda (a) (dona a)) '(9 8 7 6 5 4 3 2 1))))

(define (3-digit-number? num) (and (>= num 100) (< num 1000)))

(define (3-digit-factors num)
  (let iter ((a 1) (b num) (d 2))
    (cond [(and (3-digit-number? a) (3-digit-number? b)) (list a b)]
          [(zero? (remainder b d)) (iter (* a d) (/ b d) d)]
          [(= b 1) '()]
          ;[(> a b) '()]
          [else (iter a b (+ d 1))])))

(map 3-digit-factors (6-digit-palindrome-numbers))

(define (kame l)
  (if (null? (car l))
      (kame (cdr l))
      (car l)))

(kame (map 3-digit-factors (6-digit-palindrome-numbers)))
