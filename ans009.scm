;;;
;;; Project Euler - Problem 9
;;;

#|
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
|#

#|
a < b < c の場合は 3 * a < 1000 なので 1 <= a <= 333
|#

(use srfi-1)

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (pythagorean-triplet? a b c)
  (= (sum-of-squares a b) (square c)))

(define the-pythagorean-triplets
  (filter
   (lambda (t) (apply pythagorean-triplet? t))
   (apply append
          (map (lambda (a)
                 (map (lambda (b) (list a b (- 1000 a b))) (iota 333 a)))
               (iota 333 1)))))

(apply * (car the-pythagorean-triplets))
; => 31875000

;;;
;;; 継続を使用して見つかったらループを終了
;;;

(call/cc
  (lambda (k)
    (for-each
     (lambda (a)
       (for-each
        (lambda (b) 
          (let ((c (- 1000 a b)))
            (when (pythagorean-triplet? a b c) (k (* a b c)))))
        (iota 333 a)))
     (iota 333 1))
    #f))

;;;
;;; Python だと次のように書ける
;;;
#|
def a9():
    """Return a*b*c of the Pythagorean triplet where a+b+c==1000"""
    for a in range(1,500):
        for b in range(1,500):
            c=1000-(a+b)
            if a**2+b**2==c**2:
                return a*b*c

print a9()
|#

;;;
;;; マクロで for を定義
;;;

(define-syntax for
  (syntax-rules (in)
    [(_ (e1 in e2) e3 ...) (for-each (lambda (e1) e3 ...) e2)]))

(call/cc
  (lambda (k)
    (for (a in (iota 333 1))
      (for (b in (iota 333 a))
        (let ((c (- 1000 a b)))
          (when (pythagorean-triplet? a b c) (k (* a b c))))))
    #f))
