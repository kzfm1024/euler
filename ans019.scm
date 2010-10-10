;;; 
;;; Project Euler - Problem 19
;;; 

#|
You are given the following information, but you may prefer to do some
research for yourself.

- 1 Jan 1900 was a Monday.
- Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
- A leap year occurs on any year evenly divisible by 4, but not on a
  century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?
|#

(use srfi-1)

(define (day-of-week nth-day)
  (let ((r (remainder nth-day 7)))
    (cond [(= r 1) 'Monday]
          [(= r 2) 'Tuesday]
           [(= r 3) 'Wednesday]
           [(= r 4) 'Thursday]
           [(= r 5) 'Friday]
           [(= r 6) 'Saturday]
           [else    'Sunday])))

(define (leap-year? y)
  (cond [(zero? (remainder y 400)) #t]
        [(zero? (remainder y 100)) #f]
        [(zero? (remainder y 4)) #t]
        [else #f]))

(define (num-days-year y)
  (if (leap-year? y) 366 365))

(define (num-days-month y m)
  (cond [(= m 1) 31]
        [(= m 2) (if (leap-year? y) 29 28)]
        [(= m 3) 31]
        [(= m 4) 30]
        [(= m 5) 31]
        [(= m 6) 30]
        [(= m 7) 31]
        [(= m 8) 31]
        [(= m 9) 30]
        [(= m 10) 31]
        [(= m 11) 30]
        [(= m 12) 31]
        [else (error "invalid month:" m)]))

(define (nth-day y m d)
  (define (total-num-days-year y1 y2)
    (if (= y1 y2)
        0
        (+ (num-days-year y1)
           (total-num-days-year (+ y1 1) y2))))
  (define (total-num-days-month y m1 m2)
    (if (= m1 m2)
        0
        (+ (num-days-month y m1)
           (total-num-days-month y (+ m1 1) m2))))
  (let ((y0 1900) (m0 1))
    (unless (>= y y0)
      (error "invalid year:" y))
    (unless (and (>= d 1) (<= d (num-days-month y m)))
      (error "invalid day:" d))
    (+ (total-num-days-year y0 y)
       (total-num-days-month y m0 m)
       d)))

(define-syntax map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (map (lambda (e1) e3 ...) e2)]))

(define-syntax append-map2
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (append-map (lambda (e1) e3 ...) e2)]))

(length
  (filter (lambda (wd) (eq? wd 'Sunday))
    (append-map2 (y in (iota 100 1901))
      (map2
        (m in (iota 12 1)) (day-of-week (nth-day y m 1))))))
;=> 171

;;----------------------------------------------------------------------

(define (leap-year? y)
  (or (zero? (remainder y 400))
      (and (zero? (remainder y 4))
           (not (zero? (remainder y 100))))))

(filter (lambda (date) (eq? (cadddr date) 'Sunday))
  (append-map2 (y in (iota 100 1901))
    (map2
      (m in (iota 12 1)) (list y m 1 (day-of-week (nth-day y m 1))))))

(define *date*
  (append-map2 (y in (iota 100 1901))
               (map2
                (m in (iota 12 1)) (day-of-week (nth-day y m 1)))))

;;----------------------------------------------------------------------

(use srfi-1)
(use srfi-19)

(define-syntax for
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (for-each (lambda (e1) e3 ...) e2)]))

(let ((count 0))
  (for (y in (iota 100 1901))
    (for (m in (iota 12 1))
      (when (= (date-week-day (make-date 0 0 0 0 1 m y 0)) 0)
        (set! count (+ count 1)))))
  count)
