;;;
;;; Project Euler - Problem 38
;;;

(use srfi-1)
(use util.combinations)

(define number->digits
  (lambda (n) (string->list (number->string n))))

(define digits->number
  (lambda (l)
    (string->number
     (apply string-append
            (map (lambda (x) (number->string (digit->integer x))) l)))))

(define product?
  (lambda (d1 i d2)
    (let ([di (number->digits (* i (digits->number d1)))])
      (cond [(null? d2) #t]
            [(< (length d2) (length di)) #f]
            [else (receive (a b) (split-at d2 (length di))
                    (if (equal? di a)
                        (product? d1 (+ i 1) b)
                        #f))]))))

(define concatenated-product?
  (lambda (digits)
    (or (receive (a b) (split-at digits 1) (product? a 2 b))
        (receive (a b) (split-at digits 2) (product? a 2 b))
        (receive (a b) (split-at digits 3) (product? a 2 b))
        (receive (a b) (split-at digits 4) (product? a 2 b)))))

(apply max 
       (map (lambda (d) (digits->number d))
            (filter concatenated-product?
                    (permutations (string->list "987654321")))))
;;=> 932718654
