;;;
;;; Project Euler - Problem 30
;;;

(use srfi-1)

(define sum-of-nth-powers-of-digits
  (lambda (nth)
    (lambda (x)
      (apply +
             (map (lambda (d) (expt (digit->integer d) nth))
                  (string->list (number->string x)))))))

(define range
  (lambda (start end)
    (iota (+ (- end start) 1) start)))

(apply +
       (filter
        (lambda (x)
          (= x ((sum-of-nth-powers-of-digits 5) x)))
        (range 2 (* 6 (expt 9 5)))))
;;=> 443839

;;----------------------------------------------------------------------

(map (lambda (x) (expt x 5)) (iota 9 1))
;=> (1 32 243 1024 3125 7776 16807 32768 59049)

;; 4 桁の最大値 9999 の各桁を 5 乗した数
(* 4 (expt 9 5)) ;=> 236196

;; 5 桁の最大値 99999 の各桁を 5 乗した数
(* 5 (expt 9 5)) ;=> 295245

;; 6 桁の最大値 999999 の各桁を 5 乗した数
(* 6 (expt 9 5)) ;=> 354294
