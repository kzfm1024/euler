;;;
;;; Project Euler - Problem 31
;;;

(define count-change
  (lambda (amount coins)
    (let cc [(amount amount) (coins coins)]
      (cond [(= amount 0) 1]
            [(or (< amount 0) (null? coins)) 0]
            [else (+ (cc amount (cdr coins))
                     (cc (- amount (car coins)) coins))]))))

(count-change 200 '(200 100 50 20 10 5 2 1)) ;=> 73682
              
;;---------------------------------------------------------------------------
;;
;; SICP 変更版
;;
(define (count-change amount)
  (cc amount 8))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 10)
        ((= kinds-of-coins 5) 20)
        ((= kinds-of-coins 6) 50)
        ((= kinds-of-coins 7) 100)
        ((= kinds-of-coins 8) 200)))

(count-change 200)
;;=> 73682
