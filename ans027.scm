;;;
;;; Project Euler - Problem 27
;;;

(use srfi-1)
(use util.stream)
(use gauche.collection)
(use util.combinations)

(define prime?
  (lambda (n)
    (letrec ((smallest-divisor
              (lambda (d)
                (cond [(> (* d d) n) n]
                      [(= (remainder n d) 0) d]
                      [else (smallest-divisor (+ d 1))]))))
      (and (>= n 2)
           (= n (smallest-divisor 2))))))

(define quadratic-formula
  (lambda (a b)
    (lambda (n)
      (+ (* n n) (* a n) b))))

(define number-of-primes
  (lambda (a b)
    (length
     (stream->list
      (stream-take-while 
       (lambda (n) (prime? ((quadratic-formula a b) n)))
       (stream-iota -1))))))

(apply *
       (cdr
        (find-max
         (map (lambda (c) (cons (apply number-of-primes c) c))
              (cartesian-product (list (iota 1999 -999)
                                       (iota 1999 -999))))
         :key car)))
;;=> -59231

;(time (apply * (cdr (find-max (map (lambda (c) (cons (apply number-of-p ...
; real  99.527
; user  98.610
; sys    0.650

;;----------------------------------------------------------------------

;; (iota 1999 -999) => (-999 .. 999)
;; (iota 999 -999)  => (-999 ..  -1)
;; (iota 1000)      => (0    .. 999)

(define thunk0
  (lambda ()
    (find-max
     (map (lambda (c) (cons (apply number-of-primes c) c))
          (cartesian-product (list (iota 1999 -999)
                                   (iota 1999 -999))))
     :key car)))

(define thunk1
  (lambda ()
    (find-max
     (map (lambda (c) (cons (apply number-of-primes c) c))
          (cartesian-product (list (iota 1999 -999)
                                   (iota 999 -999))))
     :key car)))

(define thunk2
  (lambda ()
    (find-max
     (map (lambda (c) (cons (apply number-of-primes c) c))
          (cartesian-product (list (iota 1999 -999)
                                   (iota 1000))))
     :key car)))

(use gauche.time)

(begin (time (thunk0))
       (time (thunk1))
       (time (thunk2)))

;;----------------------------------------------------------------------

(define-syntax map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (map (lambda (e1) e3 ...) e2)]))

(define-syntax append-map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (append-map (lambda (e1) e3 ...) e2)]))

(apply *
       (cdr
        (find-max
         (append-map2 (a in (iota 1999 -999))
           (map2 (b in (iota 1999 -999))
             (list (number-of-primes a b) a b)))
         :key car)))
