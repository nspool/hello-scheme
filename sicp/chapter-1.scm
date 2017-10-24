;; definition of a common procedure
(define (square x) (* x x))

;; sum of squares
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (cube x) (* x x x))

(define (howdy) (display "Howdy!"))
(define (hello-world) (display "Hello, world!"))

;; `Fully reduce then expand' : Normal order
;; `Evaluate the args then apply' : Applicative-order evaluation

;; Recursive Newton's Method for square root
(define epsilon 0.00000001)

(define (find-square-root x)
  (define (average guess y)
    (/ (+ guess y) 2))

  (define (improve guess)
    (display guess)(newline)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) epsilon))

  ;; |x| is known as the radicand
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Recursive Newton's Method for cube root
(define (find-cube-root x)
  (define (improve-cube y)
    (display y)(newline)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
  
  (define (good-enough-cube? guess)
    (< (abs (- (cube guess) x)) epsilon))

  (define (cube-iter guess)
    (if (good-enough-cube? guess)
        guess
        (cube-iter (improve-cube guess))))

  (cube-iter 1.0))


;; Using let to create local variables

;;; SICP Chapter 1 - Exercises
;
;;; Use C-x C-e or M-z to evaluate the current line
;;; On the Mac, both command and escape can be used for meta
;
;* 1.1
;10
;12
;8
;3
;6
;a
;b
;19
;#f
;4
;16
;6
;16
;
;* 1.2
;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;(/ (- 15 (/ 1 5)) -60)
;
;* 1.3
;(define (my-sum a b c) 
;(cond ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
;      ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
;      (t (+ (* b b) (* a a)))))
;
;* 1.4


(define (fast-expr b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expr b (/ n 2))))
        (else (* b (fast-expr b (- n 1))))))

(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))


