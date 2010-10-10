;;;
;;; Project Euler - Problem 33
;;;

(use srfi-1)
(use util.combinations)

(define range
  (lambda (start end)
    (iota (+ (- end start) 1) start)))

(define number->digits
  (lambda (n) (string->list (number->string n))))
  
(define digits->number
  (lambda (l)
    (string->number
     (apply string-append
            (map (lambda (x) (number->string (digit->integer x))) l)))))

(define delete1
  (lambda (x l . opt)
    (let ((elt= equal?))
      (unless (null? opt)
        (set! elt= (car opt)))
      (cond ((null? l) '())
            ((elt= x (car l)) (cdr l))
            (else (cons (car l) (delete1 x (cdr l))))))))

(define make-frac
  (lambda (n d) (list n d)))

(define numer (lambda (frac) (car frac)))

(define denom (lambda (frac) (cadr frac)))

(define reduce-same-digit
  (lambda (frac)
    (define delete-same-digit
      (lambda (n d)
        (let iter ((n n) (d d) (n1 '()))
          (cond ((null? n) (list n1 d))
                ((memv (car n) d)
                 (iter (cdr n) (delete1 (car n) d eqv?) n1))
                (else (iter (cdr n) d (cons (car n) n1)))))))
    (let* ((fd (delete-same-digit (number->digits (numer frac))               
                                  (number->digits (denom frac))))
           (nd (car fd))
           (dd (cadr fd)))
      (make-frac
       (if (null? nd) 1 (digits->number nd))
       (if (null? dd) 1 (digits->number dd))))))

(define curious-fraction?
  (lambda (frac)
    (let ((frac2 (reduce-same-digit frac)))
      (let ((n (numer frac))
            (d (denom frac))
            (n2 (numer frac2))
            (d2 (denom frac2)))
        (and (not (zero? (remainder n 10)))
             (not (zero? (remainder d 10)))
             (not (= n2 n))
             (not (= d2 d))
             (= (/ n2 d2) (/ n d)))))))

(apply
 * 
 (map (lambda (x) (/ (numer x) (denom x)))
      (filter
       (lambda (x) (and (< (numer x) (denom x))
                        (curious-fraction? x)))
       (cartesian-product (list (range 10 99) (range 10 99))))))
;;=> 1/100
