;;;
;;; Project Euler - Problem 2
;;;

#|
Each new term in the Fibonacci sequence is generated by adding the previous
two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

Find the sum of all the even-valued terms in the sequence which do not exceed
four million.
|#

(use srfi-1)
(use util.stream)

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define the-sequence
  (cddr (stream->list
         (stream-take-while
          (lambda (n) (<= n 4000000)) (fibgen 0 1)))))

(fold + 0 (filter even? the-sequence))
;=> 4613732

#|
(define the-sequence
  (delete-duplicates
   (filter positive?
           (stream->list
            (stream-take-while
             (lambda (n) (<= n 4000000)) (fibgen 0 1))))))

(define the-sequence
  (drop (stream->list
         (stream-take-while
          (lambda (n) (<= n 4000000)) (fibgen 0 1)))
        2))
|#