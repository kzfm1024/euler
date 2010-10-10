;;;
;;; Project Euler - Problem 28
;;;

(use srfi-1)

(define (make-grid n)
  (let ((grid (make-vector n #f)))
    (for-each
     (lambda (i)
       (vector-set! grid i (make-vector n #f)))
     (iota n))
    grid))

(define (grid-clear! grid)
  (for-each 
   (lambda (i)
     (vector-fill! (vector-ref grid i) #f))
   (iota (vector-length grid)))
  grid)

(define (grid-width grid) (vector-length grid))

(define (grid-height grid) (vector-length (vector-ref grid 0)))

(define (grid-ref grid i j)
  (vector-ref (vector-ref grid i) j))

(define (grid-set! grid i j val)
  (vector-set! (vector-ref grid i) j val))

(define (grid-inside? grid i j)
  (and (>= i 0)
       (<  i (grid-height grid))
       (>= j 0)
       (<  j (grid-width grid))))

(define (grid-vacant? grid i j) (not (grid-ref grid i j)))
  
(define (grid-setup! grid)
  (define next
    (let ((dir 'LEFT))
      (lambda (i j val)
        (cond
          ((eq? dir 'LEFT)
           (if (and (grid-inside? grid i (- j 1))
                    (grid-vacant? grid i (- j 1)))
               (list i (- j 1) (- val 1))
               (begin (set! dir 'DOWN)
                      (next i j val))))
          ((eq? dir 'DOWN)
           (if (and (grid-inside? grid (+ i 1) j)
                    (grid-vacant? grid (+ i 1) j))
               (list (+ i 1) j (- val 1))
               (begin (set! dir 'RIGHT)
                      (next i j val))))
          ((eq? dir 'RIGHT)
           (if (and (grid-inside? grid i (+ j 1))
                    (grid-vacant? grid i (+ j 1)))
               (list i (+ j 1) (- val 1))
               (begin (set! dir 'UP)
                      (next i j val))))
          ((eq? dir 'UP)
           (if (and (grid-inside? grid (- i 1) j)
                    (grid-vacant? grid (- i 1) j))
               (list (- i 1) j (- val 1))
               (begin (set! dir 'LEFT)
                      (next i j val))))))))
  (grid-clear! grid)
  (let iter ((i 0)
             (j (- (grid-width grid) 1))
             (val (* (grid-width grid) (grid-height grid))))
    (grid-set! grid i j val)
    (cond [(= val 1) grid]
          [else (apply iter (next i j val))])))

(define (diagonals n)
  (append 
   (map (lambda (x) (cons x x)) (iota n))
   (filter 
    (lambda (p)
      (not (= (car p) (cdr p))))
    (map (lambda (x) (cons x (- n x 1))) (iota n)))))

(define (grid-sum-of-diagonals grid)
  (apply +
         (map (lambda (d) (grid-ref grid (car d) (cdr d))) 
              (diagonals (grid-width grid)))))

(grid-sum-of-diagonals (grid-setup! (make-grid 1001)))
;;=> 669171001

;;----------------------------------------------------------------------

(grid-sum-of-diagonals (grid-setup! (make-grid 5)))
;;=> 101
