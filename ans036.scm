;;;
;;; Project Euler - Problem 36
;;;

(use srfi-1)

(define range
  (lambda (start end)
    (iota (+ (- end start) 1) start)))

(define palindromic-decimal-number?
  (lambda (n)
    (let ((l (string->list (number->string n))))
      (equal? l (reverse l)))))

(define palindromic-binary-number?
  (lambda (n)
    (let ((l (string->list (number->string n 2))))
      (equal? l (reverse l)))))

(apply +
       (filter
        (lambda (n)
          (and (palindromic-decimal-number? n)
               (palindromic-binary-number? n)))
        (range 1 999999)))
;;=> 872187
