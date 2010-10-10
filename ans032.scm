;;;
;;; Project Euler - Problem 32
;;;

(use srfi-1)
(use util.combinations)

(define make-3-numbers
  (lambda (digits d1 d2)
    (receive (n1 tmp) (split-at digits d1)
      (receive (n2 n3) (split-at tmp d2)
        (list (string->number (list->string n1))
              (string->number (list->string n2))
              (string->number (list->string n3)))))))

(define multiplicand
  (lambda (numbers) (car numbers)))

(define multiplier
  (lambda (numbers) (cadr numbers)))

(define product
  (lambda (numbers) (caddr numbers)))

(define multiplication?
  (lambda (numbers)
    (= (product numbers)
       (* (multiplicand numbers) (multiplier numbers)))))
     
(apply
 + 
 (delete-duplicates
  (map
   product
   (let1 perm (permutations (string->list "123456789"))
     (append
      (filter multiplication?
              (map (lambda (x) (make-3-numbers x 1 4)) perm))
      (filter multiplication?
              (map (lambda (x) (make-3-numbers x 2 3)) perm)))))))
;;=> 45228 
