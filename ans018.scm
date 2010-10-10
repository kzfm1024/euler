;;; 
;;; Project Euler - Problem 18
;;;

#|
By starting at the top of the triangle below and moving to adjacent
numbers on the row below, the maximum total from top to bottom is 23.

   3
  7 4
 2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

              75
             95 64
            17 47 82
           18 35 87 10
          20 04 82 47 65
         19 01 23 75 03 34
        88 02 77 73 07 63 67
       99 65 04 28 06 16 70 92
      41 41 26 56 83 40 80 70 33
     41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
   70 11 33 28 77 73 17 78 39 68 17 57
  91 71 52 38 17 14 91 43 58 50 27 29 48
 63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this
problem by trying every route. However, Problem 67, is the same 
challenge with a triangle containing one-hundred rows; it cannot be
solved by brute force, and requires a clever method! ;o)
|#

(use srfi-1)

(define the-triangle
  '#(#(75)
     #(95 64)
     #(17 47 82)
     #(18 35 87 10)
     #(20 04 82 47 65)
     #(19 01 23 75 03 34)
     #(88 02 77 73 07 63 67)
     #(99 65 04 28 06 16 70 92)
     #(41 41 26 56 83 40 80 70 33)
     #(41 48 72 33 47 32 37 16 94 29)
     #(53 71 44 65 25 43 91 52 97 51 14)
     #(70 11 33 28 77 73 17 78 39 68 17 57)
     #(91 71 52 38 17 14 91 43 58 50 27 29 48)
     #(63 66 04 68 89 53 67 30 73 16 69 87 40 31)
     #(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(define (triangle-height tri) (vector-length tri))

(define (triangle-row tri i) (vector-ref tri i))

(define (triangle-ref tri i j) (vector-ref (vector-ref tri i) j))

(define (make-route j total) (list (list j) total))

(define (add-route j val route)
  (list (cons j (car route)) (+ val (cadr route))))

(define (route-total route) (cadr route))

(define (find-max-total tri)
  (define (reduce-routes i j routes)
    (cond [(null? (cdr routes)) '()]
          [else (let ((r1 (car routes)) (r2 (cadr routes)))
                  (cons (if (> (route-total r1) (route-total r2))
                            (add-route j (triangle-ref tri i j) r1)
                            (add-route j (triangle-ref tri i j) r2))
                        (reduce-routes i (+ j 1) (cdr routes))))]))
  (define (iter i routes)
    (if (zero? i)
        (car routes)
        (iter (- i 1) (reduce-routes (- i 1) 0 routes))))
  (let ((ibottom (- (triangle-height tri) 1)))
    (iter ibottom
         (map
          (lambda (j) (make-route j (triangle-ref tri ibottom j)))
          (iota (+ ibottom 1))))))

(find-max-total the-triangle)
;=> ((0 1 2 2 2 3 3 3 4 5 6 7 8 8 9) 1074)
