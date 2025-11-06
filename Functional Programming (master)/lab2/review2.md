% Лабораторная работа № 2. Символьные вычисления и метапрограммирование на языках семейства Лисп
% 16 октября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Знакомство со средствами метапрограммирования языков семейства Лисп (Scheme) на примерах.

# Индивидуальный вариант
Абстрактный синтаксис арифметических выражений:

`Expr → Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr | VARNAME`

Написать процедуру (ratio-polynoms expr) -> expr, которая преобразует выражение в полином 
(если в нём нет операции деления) или в отношение двух полиномов (если операция деления есть).

# Реализация

```scheme
#lang racket

(define (derivative expr)
  (cond
    ((number? expr) 0)

    ((eq? expr 'x) 1)

    ((and (pair? expr) (eq? (car expr) '-))
     (list '- (derivative (cadr expr))))

    ((and (pair? expr) (eq? (car expr) '+))
     (cons '+ (map derivative (cdr expr))))

    ((and (list? expr) (eq? (car expr) '*))
     (let ((factors (cdr expr)))
       (cond
         ((= (length factors) 2)
          (let ((u (car factors)) (v (cadr factors)))
            (list '+
                  (list '* (derivative u) v)
                  (list '* u (derivative v)))))
         (else
          (derivative (list '* (car factors) (cons '* (cdr factors))))))))

    ((and (pair? expr) (eq? (car expr) '/))
     (let ((f (cadr expr))
           (g (caddr expr)))
       (list '/ (list '- (list '* (derivative f) g)
                          (list '* f (derivative g)))
             (list 'expt g 2))))

    ((and (pair? expr) (eq? (car expr) 'expt))
     (let ((base (cadr expr))
           (pow (caddr expr)))
       (cond
         ((and (eq? base 'x) (number? pow))
          (list '* pow (list 'expt 'x (- pow 1))))

         ((and (number? base) (eq? pow 'x))
          (list '* (list 'expt base 'x) (list 'log base)))
         
         (else (list '* (list 'expt base pow)
                         (list 'log base)
                         (derivative pow))))))

    ((and (pair? expr) (eq? (car expr) 'exp))
     (list '* (derivative (cadr expr)) expr))

    ((and (pair? expr) (eq? (car expr) 'ln))
     (list '/ (derivative (cadr expr)) (cadr expr)))

    ((and (pair? expr) (eq? (car expr) 'sin))
     (list '* (derivative (cadr expr)) (list 'cos (cadr expr))))

    ((and (pair? expr) (eq? (car expr) 'cos))
     (list '* (derivative (cadr expr)) (list '- (list 'sin (cadr expr)))))
    
    (else (error "Unknown expression" expr))))
    
(define-syntax define-data
  (syntax-rules ()
    ((define-data data-name ((name field1 ...) ...))
     (begin
       (eval (list 'define
                   'name
                   (lambda (field1 ...)
                     (list (list 'd-name 'data-name) 
                           (list 't-name 'name)
                           (list 'field1 field1) ...)))
             (interaction-environment))
       ...
       (eval (list 'define
                   (string->symbol 
                    (string-append (symbol->string 'data-name) "?"))
                   (lambda (x)
                     (and (list? x) (>= (length x) 2)
                          (let ((d-nameres (assoc 'd-name x)))
                            (and d-nameres 
                                 (equal? (cadr d-nameres) 'data-name))))))
             (interaction-environment))))))

(define-syntax match
  (syntax-rules (-> if)
    ((match x) x)
    ((match x ((name field1 ...) if guard -> expr) more ...)
     (let ((tmp x))
       (if (equal? (cadadr tmp) 'name)
           (let ((field1 (cadr (assoc 'field1 tmp))) ...)
             (if guard
                 expr
                 (match tmp more ...)))
           (match tmp more ...))))
    ((match x ((name field1 ...) -> expr) more ...)
     (let ((tmp x))
       (if (equal? (cadadr tmp) 'name)
           (let ((field1 (cadr (assoc 'field1 tmp))) ...)
             expr)
           (match tmp more ...))))))


(display "-------Индивидуальный вариант--------") (newline)

(define-data expr
  ((add left right)
   (sub left right)
   (mul left right)
   (div left right)
   (var name)
   (num value)))

(define (expand expr)
  (match expr
    ((num value) -> (num value))
    ((var name) -> (var name))
    ((add left right) -> (add (expand left) (expand right)))
    ((sub left right) -> (sub (expand left) (expand right)))
    ((mul left right) -> 
     (let ((left-exp (expand left))
           (right-exp (expand right)))
       (distribute left-exp right-exp)))
    ((div left right) -> (div (expand left) (expand right)))))

(define (distribute l r)
  (match l
    ((add left right) -> (add (distribute left r) (distribute right r)))
    ((sub left right) -> (sub (distribute left r) (distribute right r)))
    ((mul left right) -> (mul r (distribute left right)))
    ((div left right) -> (div (distribute left r) (distribute right r)))
    ((num value) if (= value 1) ->
      (match r
       ((var name) -> r)
       ((num value) -> r)))
    ((num value) -> 
     (match r
       ((add left right) -> (add (distribute l left) (distribute l right)))
       ((sub left right) -> (sub (distribute l left) (distribute l right)))
       ((mul left right) -> (mul l (distribute left right)))
       ((div left right) -> (div (distribute l left) (distribute l right)))
       ((var name) -> (mul l r))
       ((num value) if (= value 1) -> l)
       ((num value) -> (mul l r))))
    ((var name) -> 
     (match r
       ((add left right) -> (add (distribute l left) (distribute l right)))
       ((sub left right) -> (sub (distribute l left) (distribute l right)))
       ((mul left right) -> (mul l (distribute left right)))
       ((div left right) -> (div (distribute l left) (distribute l right)))
       ((var name) -> (mul l r))
       ((num value) if (= value 1) -> l)
       ((num value) -> (mul l r))))));)

(define (ratio-polynoms expr)
  (define (to-fraction e)
    (match e
      ((num value) -> (cons (num value) (num 1)))
      ((var name) -> (cons (var name) (num 1)))
      ((add left right) -> 
       (let ((f1 (to-fraction left))
             (f2 (to-fraction right)))
         (cons (add (mul (car f1) (cdr f2)) 
                    (mul (car f2) (cdr f1)))
               (mul (cdr f1) (cdr f2)))))
      ((sub left right) -> 
       (let ((f1 (to-fraction left))
             (f2 (to-fraction right)))
         (cons (sub (mul (car f1) (cdr f2)) 
                    (mul (car f2) (cdr f1)))
               (mul (cdr f1) (cdr f2)))))
      ((mul left right) -> 
       (let ((f1 (to-fraction left))
             (f2 (to-fraction right)))
            (cons (mul (car f1) (car f2))
               (mul (cdr f1) (cdr f2)))))
      ((div left right) -> 
       (let ((f1 (to-fraction left))
             (f2 (to-fraction right)))
         (cons (mul (car f1) (cdr f2))
               (mul (cdr f1) (car f2)))))))
  
  (let ((frac (to-fraction expr)))
    (let ((numerator (expand (car frac)))
          (denominator (expand (cdr frac))))
      (begin
      (if (equal? denominator (num 1))
          numerator
          (div numerator denominator))))))

(define (expr->compact expr)
  (match expr
    ((num value) -> value)
    ((var name) -> name)
    ((add left right) -> (list '+ (expr->compact left) (expr->compact right)))
    ((sub left right) -> (list '- (expr->compact left) (expr->compact right)))
    ((mul left right) -> (list '* (expr->compact left) (expr->compact right)))
    ((div left right) -> (list '/ (expr->compact left) (expr->compact right)))))

(define (compact->expr expr)
  (cond
    ((number? expr) (num expr))
    ((symbol? expr) (var expr))
    ((and (list? expr) (not (null? expr)))
     (let ((op (car expr))
           (args (cdr expr)))
       (cond
         ((eq? op '+) (add (compact->expr (car args)) (compact->expr (cadr args))))
         ((eq? op '-) (sub (compact->expr (car args)) (compact->expr (cadr args))))
         ((eq? op '*) (mul (compact->expr (car args)) (compact->expr (cadr args))))
         ((eq? op '/) (div (compact->expr (car args)) (compact->expr (cadr args))))
         (else (error "Unknown operator:" op)))))
    (else (error "Invalid expression:" expr))))
```

# Тестирование

```
(define (check-equal? expr expected msg)
  (let ((result (derivative expr)))
    (display "TEST: ") (display msg) (newline)
    (display " input: ") (write expr) (newline)
    (display " expect: ") (write expected) (newline)
    (display " result: ") (write result) (newline)
    (if (equal? result expected)
        (display " => OK") 
        (display " => FAIL"))
    (newline)(newline)))

(check-equal? '2 0 "d(2)")
(check-equal? 'x 1 "d(x)")
(check-equal? '(- x) '(- 1) "d(-x)")
(check-equal? '(* -4 x) '(+ (* 0 x) (* -4 1)) "d(-4x)")
(check-equal? '(* 10 x) '(+ (* 0 x) (* 10 1)) "d(10x)")
(check-equal? '(- (* 2 x) 3) '(- (+ (* 0 x) (* 2 1))) "d(2x-3)")
(check-equal? '(expt x 10) '(* 10 (expt x 9)) "d(x^10)")
(check-equal? '(* 2 (expt x 5)) '(+ (* 0 (expt x 5)) (* 2 (* 5 (expt x 4)))) "d(2x^5)")
(check-equal? '(- x 2) '(- 1) "d(x-2)")
(check-equal? '(* 5 x) '(+ (* 0 x) (* 5 1)) "d(5x)")
(check-equal? '(cos x) '(* 1 (- (sin x))) "d(cos x)")
(check-equal? '(sin x) '(* 1 (cos x)) "d(sin x)")
(check-equal? '(exp x) '(* 1 (exp x)) "d(e^x)")
(check-equal? '(* 2 (exp x)) '(+ (* 0 (exp x)) (* 2 (* 1 (exp x)))) "d(2e^x)")
(check-equal? '(ln x) '(/ 1 x) "d(ln x)")
(check-equal? '(* 3 (ln x)) '(+ (* 0 (ln x)) (* 3 (/ 1 x))) "d(3ln x)")
(check-equal? '(+ (expt x 3) (expt x 2))
              '(+ (* 3 (expt x 2)) (* 2 (expt x 1)))
              "d(x^3 + x^2)")
(check-equal? '(- (* 2 (expt x 3)) (* 2 (expt x 2)))
              '(- (+ (* 0 (expt x 3)) (* 2 (* 3 (expt x 2)))))
              "d(2x^3 - 2x^2)")
(check-equal? '(/ 3 x)
              '(/ (- (* 0 x) (* 3 1)) (expt x 2))
              "d(3/x)")
(check-equal? '(/ 3 (* 2 (expt x 2)))
              '(/ (- (* 0 (* 2 (expt x 2)))(* 3 (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1))))))
              (expt (* 2 (expt x 2)) 2))
              "d(3/(2x^2))")
(check-equal? '(* 2 (sin x) (cos x))
              '(+ (* 0 (* (sin x) (cos x))) (* 2 (+ (* (* 1 (cos x)) (cos x)) (* (sin x) (* 1 (- (sin x)))))))
              "d(2 sin x cos x)")
(check-equal? '(* 2 (exp x) (sin x) (cos x))
              '(+ (* 0 (* (exp x) (sin x) (cos x))) (* 2 (+ (* (* 1 (exp x)) (* (sin x) (cos x)))
               (* (exp x) (+ (* (* 1 (cos x)) (cos x)) (* (sin x) (* 1 (- (sin x)))))))))
              "d(2 e^x sin x cos x)")
(check-equal? '(sin (* 2 x)) '(* (+ (* 0 x) (* 2 1)) (cos (* 2 x))) "d(sin 2x)")
(check-equal? '(cos (expt x 2))
              '(* (* 2 (expt x 1)) (- (sin (expt x 2))))
              "d(cos(x^2))")
(check-equal? '(sin (ln (expt x 2)))
              '(* (/ (* 2 (expt x 1)) (expt x 2)) (cos (ln (expt x 2))))
              "d(sin(ln(x^2)))")
(check-equal? '(+ (sin (* 2 x)) (cos (expt x 2)))
              '(+ (* (+ (* 0 x) (* 2 1)) (cos (* 2 x))) (* (* 2 (expt x 1)) (- (sin (expt x 2)))))
              "d(sin(2x)+cos(x^2))")
(check-equal? '(* (sin (* 2 x)) (cos (expt x 2)))
              '(+ (* (* (+ (* 0 x) (* 2 1)) (cos (* 2 x))) (cos (expt x 2))) (* (sin (* 2 x))
               (* (* 2 (expt x 1)) (- (sin (expt x 2))))))
              "d(sin(2x)*cos(x^2))")
             
(define (run-test test-name actual expected)
  (display test-name)
  (display ": ")
  (if (equal? actual expected)
      (begin
        (display "PASS")
        (newline)
        #t)
      (begin
        (display "FAIL")
        (newline)
        (display "  Expected: ") (display expected) (newline)
        (display "  Actual:   ") (display actual) (newline)
        #f)))

(define (test-ratio-polynoms)
  (display "Running ratio-polynoms tests...") (newline)
  
  (let ((all-passed #t))
    
    (let ((expr1 (compact->expr '(+ (/ a b) c)))
          (expected '(/ (+ a (* c b)) b)))
      (if (run-test "a/b + c -> (a + c*b)/b" 
                    (expr->compact (ratio-polynoms expr1))
                    expected)
          #t
          (set! all-passed #f)))
    
    (let ((expr2 (compact->expr '(* (+ a c) (+ d e))))
          (expected '(+ (+ (* a d) (* a e)) (+ (* c d) (* c e)))))
      (if (run-test "(a+c)(d+e) -> a*d+a*e+c*d+c*e" 
                    (expr->compact (ratio-polynoms expr2))
                    expected)
          #t
          (set! all-passed #f)))
    
    (let ((expr3 (compact->expr '(+ a b)))
          (expected '(+ a b)))
      (if (run-test "a+b -> a+b" 
                    (expr->compact (ratio-polynoms expr3))
                    expected)
          #t
          (set! all-passed #f)))
    
    (let ((expr4 (compact->expr '(* (- a b) c)))
          (expected '(- (* a c) (* b c))))
      (if (run-test "(a-b)*c -> a*c - b*c" 
                    (expr->compact (ratio-polynoms expr4))
                    expected)
          #t
          (set! all-passed #f)))
    
    (let ((expr5 (compact->expr '(- (/ a b) (/ c d))))
          (expected '(/ (- (* a d) (* c b)) (* b d))))
      (if (run-test "a/b - c/d -> (a*d-c*b)/(b*d)" 
                    (expr->compact (ratio-polynoms expr5))
                    expected)
          #t
          (set! all-passed #f)))

    (let ((expr6 (compact->expr '(/ (/ a b) (/ c d))))
          (expected '(/ (* a d) (* b c))))
      (if (run-test "(a/b)/(c/d) -> (a*d)/(b*c)" 
                    (expr->compact (ratio-polynoms expr6))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr7 (compact->expr '(* a (/ b c))))
          (expected '(/ (* a b) c)))
      (if (run-test "a*(b/c) -> (a*b)/c" 
                    (expr->compact (ratio-polynoms expr7))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr8 (compact->expr '(/ (+ a b) 1)))
          (expected '(+ a b)))
      (if (run-test "(a+b)/1 -> a+b" 
                    (expr->compact (ratio-polynoms expr8))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr9 (compact->expr '(/ a (/ b c))))
          (expected '(/ (* a c) b)))
      (if (run-test "a/(b/c) -> (a*c)/b" 
                    (expr->compact (ratio-polynoms expr9))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr10 (compact->expr '(* (+ a b) (/ c d))))
          (expected '(/ (+ (* a c) (* b c)) d)))
      (if (run-test "(a+b)*(c/d) -> (a*c+b*c)/d" 
                    (expr->compact (ratio-polynoms expr10))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr11 (compact->expr '5))
          (expected 5))
      (if (run-test "5 -> 5" 
                    (expr->compact (ratio-polynoms expr11))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr12 (compact->expr 'x))
          (expected 'x))
      (if (run-test "x -> x" 
                    (expr->compact (ratio-polynoms expr12))
                    expected)
          #t
          (set! all-passed #f)))
 
    (let ((expr13 (compact->expr '(+ (/ a b) (/ c d))))
          (expected '(/ (+ (* a d) (* c b)) (* b d))))
      (if (run-test "a/b + c/d -> (a*d+c*b)/(b*d)" 
                    (expr->compact (ratio-polynoms expr13))
                    expected)
          #t
          (set! all-passed #f)))
    
    (let ((expr14 (compact->expr '(/ x 1)))
          (expected 'x))
      (if (run-test "x/1 -> x" 
                    (expr->compact (ratio-polynoms expr14))
                    expected)
          #t
          (set! all-passed #f)))
    и
    (let ((expr15 (compact->expr '(+ (* a (/ b c)) (/ d e))))
          (expected '(/ (+ (* e (* a b)) (* d c)) (* c e))))
      (if (run-test "a*(b/c)+d/e -> (a*b*e+d*c)/(c*e)" 
                    (expr->compact (ratio-polynoms expr15))
                    expected)
          #t
          (set! all-passed #f)))

    (let ((expr16 (compact->expr '(* (+ a b) (+ (/ c d) e))))
          (expected '(/ (+ (+ (* a c) (* a (* e d))) (+ (* b c) (* b (* e d)))) d)))
      (if (run-test "(a+b)*(c/d + e) -> (a*c + b*c + a*d*e + b*d*e)/d" 
                    (expr->compact (ratio-polynoms expr16))
                    expected)
          #t
          (set! all-passed #f)))
    
    (if all-passed
        (begin
          (display "All tests PASSED!") (newline) (display "OK"))
        (begin
          (display "Some tests FAILED!") (newline) (display "FAIL")))))

(test-ratio-polynoms)
                    
```

# Вывод
Во время выполнения данной работы было разблокировано воспоминание по написанию макросов
с первого курса бакалавриата. Помимо этого была реализована ачивка для макроса match, которая в дальнейшем
пригодилась при написании раскрытия скобок в полиноме. В целом сама задача была довольно непростой,
и я получил настоящее удовольствие, когда мое решение заработало. Было интересно!