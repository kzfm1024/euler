(use srfi-1)
(use util.combinations)

#|
(define all-true?
  (lambda (l)
    (cond [(null? l) #t]
          [else (and (car l) (all-true? (cdr l)))])))
|#

(define all-true? (lambda (l) (every (lambda (x) (if x #t #f)) l)))

(define memoize
  (lambda (f)
    (let ((table (make-hash-table)))
      (lambda (x)
        (if (hash-table-exists? table x)
            (hash-table-get table x)
            (let ((result (f x)))
              (hash-table-put! table x result)
              result))))))

(define square
  (lambda (x) (* x x)))

(define sum-of-squares
  (lambda (x y) (+ (square x) (square y))))

(define pythagorean-triplet?
  (lambda (a b c) (= (sum-of-squares a b) (square c))))

(define range
  (lambda (start end)
    (iota (- end start) start)))

(define number->digits
  (lambda (n) (string->list (number->string n))))
  
(define digits->number
  (lambda (l)
    (string->number
     (apply string-append
            (map (lambda (x) (number->string (digit->integer x))) l)))))

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

(define (number-of-divisors num)
  (if (= num 1)
      1
      (apply * (map (lambda (x) (+ (cdr x) 1))
                    (exponential-form (factors num))))))

(define sum-of-proper-divisors
  (lambda (n)
    (apply + (proper-divisors n))))

(define amicable-number?
  (lambda (a)
    (let ((b (sum-of-proper-divisors a)))
      (and (= (sum-of-proper-divisors b) a) (not (= a b))))))

(define perfect-number?
  (lambda (n)
    (= (sum-of-proper-divisors n) n)))

(define deficient-number?
  (lambda (n)
    (< (sum-of-proper-divisors n) n)))

(define abundant-number?
  (lambda (n)
    (> (sum-of-proper-divisors n) n)))

(define prime?
  (lambda (n)
    (letrec ((smallest-divisor
              (lambda (d)
                (cond [(> (* d d) n) n]
                      [(= (remainder n d) 0) d]
                      [else (smallest-divisor (+ d 1))]))))
      (and (>= n 2)
           (= n (smallest-divisor 2))))))

(define (nth-prime nth)
  (let iter ((i 1) (p 2))
    (cond [(not (prime? p)) (iter i (+ p 1))]
          [(= i nth) p]
          [else (iter (+ i 1) (+ p 1))])))

(define recurring-cycle
  (lambda (d)
    (let iter [(n 1) (d d) (qs '()) (rs '())]
      (receive (q r) (quotient&remainder n d)
        (cond [(zero? r) 0]
              [(zero? q) (iter (* n 10) d (cons q qs) (cons #f rs))]
              [(memv r (reverse rs)) => (cut length <>)]
              [else (iter (* r 10) d (cons q qs) (cons r rs))])))))

;;----------------------------------------------------------------------

(define-syntax for
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (for-each (lambda (e1) e3 ...) e2)]))

(define-syntax map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (map (lambda (e1) e3 ...) e2)]))

(define-syntax append-map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (append-map (lambda (e1) e3 ...) e2)]))
