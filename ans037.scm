;;;
;;; Project Euler - Problem 37
;;;

(use srfi-1)

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

(define all-true?
  (lambda (l)
    (cond [(null? l) #t]
          [else (and (car l) (all-true? (cdr l)))])))

(define prime?
  (lambda (n)
    (letrec ((smallest-divisor
              (lambda (d)
                (cond [(> (* d d) n) n]
                      [(= (remainder n d) 0) d]
                      [else (smallest-divisor (+ d 1))]))))
      (and (>= n 2)
           (= n (smallest-divisor 2))))))

(define truncatable-prime?
  (lambda (n)
    (let* ((digits (number->digits n))
           (len (length digits)))
      (and (> len 1)
           (all-true? (map (lambda (i)
                             (receive (d1 d2) (split-at digits i)
                               (and (prime? (digits->number d1))
                                    (prime? (digits->number d2)))))
                           (range 1 len)))
           (prime? n)))))

(define truncatable-primes
  (lambda ()
    (let iter ((n 2) (primes '()))
      (cond [(= (length primes) 11) primes]
            [(truncatable-prime? n) (iter (+ n 1) (cons n primes))]
            [else (iter (+ n 1) primes)]))))

(truncatable-primes)
;;=> (739397 3797 3137 797 373 317 313 73 53 37 23)

(apply + (truncatable-primes))
;;=> 748317
