;;;
;;; Project Euler - Problem 17
;;;

#|
If the numbers 1 to 5 are written out in words: one, two, three, four,
five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written
out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
20 letters. The use of "and" when writing out numbers is in compliance
with British usage.
|#

(use srfi-1)

(define (int->word n)
  (define (4-digit n)
    (receive (q r) (quotient&remainder n 1000)
      (cond [(and (= q 1) (= r 0)) '("one" "thousand")]
            [(= q 0) (3-digit r)]
            [else (error "must be under 1000, but got:" n)])))
  (define (3-digit n)
    (receive (q r) (quotient&remainder n 100)
      (cond [(= q 9) (append '("nine" "hundred") (and-2-digit r))]
            [(= q 8) (append '("eight" "hundred") (and-2-digit r))]
            [(= q 7) (append '("seven" "hundred") (and-2-digit r))]
            [(= q 6) (append '("six" "hundred") (and-2-digit r))]
            [(= q 5) (append '("five" "hundred") (and-2-digit r))]
            [(= q 4) (append '("four" "hundred") (and-2-digit r))]
            [(= q 3) (append '("three" "hundred") (and-2-digit r))]
            [(= q 2) (append '("two" "hundred") (and-2-digit r))]
            [(= q 1) (append '("one" "hundred") (and-2-digit r))]
            [(= q 0) (2-digit r)])))
  (define (and-2-digit n)
    (if (zero? n)
        (2-digit n)
        (cons "and" (2-digit n))))
  (define (2-digit n)
    (receive (q r) (quotient&remainder n 10)
      (cond [(= q 9) (cons "ninety" (1-digit r))]
            [(= q 8) (cons "eighty" (1-digit r))]
            [(= q 7) (cons "seventy" (1-digit r))]            
            [(= q 6) (cons "sixty" (1-digit r))]            
            [(= q 5) (cons "fifty" (1-digit r))]            
            [(= q 4) (cons "forty" (1-digit r))]            
            [(= q 3) (cons "thirty" (1-digit r))]            
            [(= q 2) (cons "twenty" (1-digit r))]            
            [(= q 1) (cond [(= r 9) '("nineteen")]
                           [(= r 8) '("eighteen")]
                           [(= r 7) '("seventeen")]
                           [(= r 6) '("sixteen")]
                           [(= r 5) '("fifteen")]
                           [(= r 4) '("fourteen")]
                           [(= r 3) '("thirteen")]
                           [(= r 2) '("twelve")]
                           [(= r 1) '("eleven")]
                           [(= r 0) '("ten")])]
            [(= q 0) (1-digit r)])))
  (define (1-digit n)
    (cond [(= n 9) '("nine")]
          [(= n 8) '("eight")]
          [(= n 7) '("seven")]
          [(= n 6) '("six")]
          [(= n 5) '("five")]
          [(= n 4) '("four")]
          [(= n 3) '("three")]
          [(= n 2) '("two")]
          [(= n 1) '("one")]
          [else '()]))
  (4-digit n))

(fold + 0 (map string-length
               (append-map (lambda (n) (int->word n)) (iota 1000 1))))
;=> 21124
