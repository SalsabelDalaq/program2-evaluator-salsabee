#lang racket

(define (eval-expr expr)
  (match expr
    [(list 'num n) (success n)]
    [(list 'add (list 'num a) (list 'num b)) (success (+ a b))]
    [(list 'sub (list 'num a) (list 'num b)) (success (- a b))]
    [(list 'mul (list 'num a) (list 'num b))
     (cond
       [(= a 0) (success 0)]
       [(= a 1) (success b)]
       [(= b 1) (success a)]
       [else (success (* a b))])]
    [(list 'div (list 'num a) (list 'num b))
     (if (= b 0)
         (failure '("Division by zero"))
         (success (/ a b)))]
    [(list 'num-div (list 'num a) (list 'num b))
     (if (= b 0)
         (failure '("Integer division by zero"))
         (success (quotient a b)))]
    [(list 'pow (list 'num a) (list 'num b))
     (cond
       [(and (= a 0) (= b 0)) (success 0)]
       [(= b 0) (success 1)]
       [(= a 0) (success 0)]
       [else (success (expt a b))])]
    [_ (failure '("Invalid expression"))]))

(define (success value) (list 'success value))
(define (failure messages) (list 'failure messages))

;; Test cases
(displayln (eval-expr '(add (num 3) (num 5))))   ;; (success 8)
(displayln (eval-expr '(sub (num 10) (num 4))))  ;; (success 6)
(displayln (eval-expr '(mul (num 6) (num 7))))   ;; (success 42)
(displayln (eval-expr '(mul (num 0) (num 7))))   ;; (success 0)
(displayln (eval-expr '(mul (num 1) (num 7))))   ;; (success 7)
(displayln (eval-expr '(div (num 8) (num 2))))   ;; (success 4.0)
(displayln (eval-expr '(num-div (num 9) (num 2)))) ;; (success 4)
(displayln (eval-expr '(pow (num 2) (num 3))))   ;; (success 8)
(displayln (eval-expr '(pow (num 0) (num 0))))   ;; (success 0)
(displayln (eval-expr '(div (num 5) (num 0))))   ;; (failure ("Division by zero"))
