;;;
;;; Project Euler - Problem 25
;;;

#|
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
|#

(define number-of-digits
  (lambda (n)
    (string-length (number->string n))))

(define first-term-in-fibs ;; to contain n digit
  (lambda (n)
    (let fib-iter [(a 1) (b 0) (count 1)]
      (if (>= (number-of-digits a) n)
          count
          (fib-iter (+ a b) a (+ count 1))))))

(first-term-in-fibs 1000)
;;=> 4782
