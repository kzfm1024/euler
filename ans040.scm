;;;
;;; Project Euler - Problem 40
;;;

(use srfi-1)
(use util.combinations)
(use gauche.collection)

(define range
  (lambda (start end)
    (iota (- end start) start)))

(define square
  (lambda (x) (* x x)))

(define sum-of-squares
  (lambda (x y) (+ (square x) (square y))))

(define pythagorean-triplet?
  (lambda (a b c) (= (sum-of-squares a b) (square c))))

(define triplets
  (lambda (perimeter)
    (let ((len (ceiling (/ perimeter 2))))
      (map (lambda (x) 
             (let ((sum (apply + x)))
               (append x (list (- perimeter sum)))))
           (cartesian-product (list (range 1 len) (range 1 len)))))))

(define pythagorean-triplets
  (lambda (perimeter)
    (filter (lambda (t)
              (and (>= (second t) (first t))
                   (apply pythagorean-triplet? t)))
            (triplets perimeter))))

(define number-of-pythagorean-triplets
  (lambda (perimeter)
    (length (pythagorean-triplets perimeter))))

(find-max
 (map (lambda (p) 
        (cons p (number-of-pythagorean-triplets p)))
      (range 1 1001))
 :key cdr)
;;=> (840 . 8)

;;----------------------------------------------------------------------

(pythagorean-triplets 120)
;;=> ((20 48 52) (24 45 51) (30 40 50))

(number-of-pythagorean-triplets 120)
;;=> 3

(number-of-pythagorean-triplets 1)
;;=> 0
