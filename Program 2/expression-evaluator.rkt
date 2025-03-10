#lang racket

;; Define success and failure as Either-like values
(define (success value) (list 'success value))
(define (failure messages) (list 'failure messages))

;; The expression evaluator function
(define (eval-expr expr)
  (match expr
    ;; Base case: Handle a number
    [(list 'num n) (success n)]

    ;; Addition: a + b
    [(list 'add (list 'num a) (list 'num b)) (success (+ a b))]

    ;; Subtraction: a - b
    [(list 'sub (list 'num a) (list 'num b)) (success (- a b))]

    ;; Multiplication: a * b
    [(list 'mul (list 'num a) (list 'num b))
     (cond
       [(= a 0) (success 0)]  ;; Any number multiplied by 0 is 0
       [(= a 1) (success b)]  ;; Anything multiplied by 1 is itself
       [(= b 1) (success a)]  ;; Same rule applies if b is 1
       [else (success (* a b))])]

    ;; Division: a / b (Real division)
    [(list 'div (list 'num a) (list 'num b))
     (if (= b 0)
         (failure '("Division by zero"))  ;; Return error if dividing by 0
         (success (/ a b)))]  ;; Real division

    ;; Integer division: a / b (using quotient)
    [(list 'num-div (list 'num a) (list 'num b))
     (if (= b 0)
         (failure '("Integer division by zero"))  ;; Error message for division by 0
         (success (quotient a b)))]  ;; Integer division using quotient

    ;; Power: a raised to the power of b
    [(list 'pow (list 'num a) (list 'num b))
     (cond
       [(and (= a 0) (= b 0)) (success 0)]  ;; 0^0 is defined as 0 here
       [(= b 0) (success 1)]  ;; Anything raised to the power of 0 is 1
       [(= a 0) (success 0)]  ;; 0 raised to any non-zero power is 0
       [else (success (expt a b))])]  ;; General power case

    ;; Default case: Return error for invalid expression
    [_ (failure '("Invalid expression"))]))

;; Test cases to validate the evaluator
(displayln (eval-expr '(add (num 3) (num 5))))  ;; (success 8)
(displayln (eval-expr '(sub (num 10) (num 4))))  ;; (success 6)
(displayln (eval-expr '(mul (num 6) (num 7))))  ;; (success 42)
(displayln (eval-expr '(mul (num 0) (num 7))))  ;; (success 0)
(displayln (eval-expr '(mul (num 1) (num 7))))  ;; (success 7)
(displayln (eval-expr '(div (num 8) (num 2))))  ;; (success 4.0)
(displayln (eval-expr '(num-div (num 9) (num 2)))) ;; (success 4)
(displayln (eval-expr '(pow (num 2) (num 3))))  ;; (success 8)
(displayln (eval-expr '(pow (num 0) (num 0))))  ;; (success 0)
(displayln (eval-expr '(div (num 5) (num 0))))  ;; (failure ("Division by zero"))
(displayln (eval-expr '(num-div (num 5) (num 0))))  ;; (failure ("Integer division by zero"))
(displayln (eval-expr '(mul (num 0) (num 0))))  ;; (success 0)
(displayln (eval-expr '(pow (num 0) (num 5))))  ;; (success 0)

