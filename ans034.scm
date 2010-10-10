;;;
;;; Project Euler - Problem 34
;;;

(use srfi-1)

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

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define sum-of-fact-of-digits
  (lambda (n)
    (apply
     + 
     (map (lambda (x) (fact (digit->integer x))) (number->digits n)))))

(define curious-number?
  (lambda (n)
    (= (sum-of-fact-of-digits n) n)))

(apply + (filter curious-number? (range 3 2540160)))
;;=> 40730

;;----------------------------------------------------------------------

(Filter curious-number? (range 3 100000))
;=> (145 40585)

(sum-of-fact-of-digits 999)
;;=> 1088640

(sum-of-fact-of-digits 9999)
;;=> 1451520

(sum-of-fact-of-digits 99999)
;;=> 1814400

(sum-of-fact-of-digits 999999)
;;=> 2177280

(sum-of-fact-of-digits 9999999)
;;=> 2540160

#|
1! = 1 
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880

(sum-of-fact-of-digits 1) => 1
(sum-of-fact-of-digits 10) => 2
(sum-of-fact-of-digits 100) => 3
(sum-of-fact-of-digits 1000) => 4
(sum-of-fact-of-digits 10000) => 5
(sum-of-fact-of-digits 100000) => 6
(sum-of-fact-of-digits 1000000) => 7
|#
