;;;
;;; Project Euler - Problem 35
;;;

(use srfi-1)
(use gauche.time)

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

(define prime?
  (lambda (n)
    (letrec ((smallest-divisor
              (lambda (d)
                (cond [(> (* d d) n) n]
                      [(= (remainder n d) 0) d]
                      [else (smallest-divisor (+ d 1))]))))
      (and (>= n 2)
           (= n (smallest-divisor 2))))))

(define all-rotations
  (lambda (digits)
    (let ((rotation
           (lambda (digits i)
             (receive (d1 d2) (split-at digits i)
               (append d2 d1)))))
      (map (lambda (i) (rotation digits i))
           (iota (length digits))))))

(define all-rotations-of-digits
  (lambda (n)
    (map digits->number (all-rotations (number->digits n)))))

(define all-true?
  (lambda (l)
    (cond [(null? l) #t]
          [else (and (car l) (all-true? (cdr l)))])))

(define circular-prime?
  (lambda (n)
    (all-true? (map prime? (all-rotations-of-digits n)))))

(time (length (filter circular-prime? (range 2 999999))))
;; real 109.736
;; user 109.410
;; sys    0.260
;;=> 55

;;----------------------------------------------------------------------

;;
;; prime? をメモ化して高速化
;;

(define memoize
  (lambda (f)
    (let ((table (make-hash-table)))
      (lambda (x)
        (if (hash-table-exists? table x)
            (hash-table-get table x)
            (let ((result (f x)))
              (hash-table-put! table x result)
              result))))))

(define memo-prime? (memoize prime?))

(define memo-circular-prime?
  (lambda (n)
    (all-true? (map memo-prime? (all-rotations-of-digits n)))))

(time (length (filter memo-circular-prime? (range 2 999999))))
;; real  72.081
;; user  71.730
;; sys    0.250
;;=> 55

;;----------------------------------------------------------------------

;;
;; マルチコア CPU を活かすためにマルチスレッド化
;;
(use gauche.threads)

(time
 (let* ((ncp (lambda (l)
               (length (filter circular-prime? l))))
        (th1 (make-thread (lambda () (ncp (range 2 500000)))))
        (th2 (make-thread (lambda () (ncp (range 500001 999999))))))
   (thread-start! th1)
   (thread-start! th2)
   (+ (thread-join! th1) (thread-join! th2))))
;; real  65.183
;; user 110.570
;; sys    0.910
;;=> 55

;;----------------------------------------------------------------------

;;
;; 2 桁以上の circular prime は 1, 3, 7, 9 の数字だけを含む
;;
(define circular-prime?
  (lambda (n)
    (let ((digits (number->digits n)))
      (and (or (= (length digits) 1)
               (all-true? (map (lambda (d) 
                                 (or (eq? d #\1)
                                     (eq? d #\3)
                                     (eq? d #\7)
                                     (eq? d #\9)))
                               digits)))
           (all-true? (map (lambda (d) (memo-prime? (digits->number d)))
                           (all-rotations digits)))))))

(time (length (filter circular-prime? (range 2 1000000))))
;; real   3.189
;; user   3.180
;; sys    0.000
;;=> 55
