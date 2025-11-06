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


;; === Полный список тестов ===
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
              '(/ (- (* 0 (* 2 (expt x 2)))(* 3 (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1))))))(expt (* 2 (expt x 2)) 2))
              "d(3/(2x^2))")
(check-equal? '(* 2 (sin x) (cos x))
              '(+ (* 0 (* (sin x) (cos x))) (* 2 (+ (* (* 1 (cos x)) (cos x)) (* (sin x) (* 1 (- (sin x)))))))
              "d(2 sin x cos x)")
(check-equal? '(* 2 (exp x) (sin x) (cos x))
              '(+ (* 0 (* (exp x) (sin x) (cos x))) (* 2 (+ (* (* 1 (exp x)) (* (sin x) (cos x))) (* (exp x) (+ (* (* 1 (cos x)) (cos x)) (* (sin x) (* 1 (- (sin x)))))))))
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
              '(+ (* (* (+ (* 0 x) (* 2 1)) (cos (* 2 x))) (cos (expt x 2))) (* (sin (* 2 x)) (* (* 2 (expt x 1)) (- (sin (expt x 2))))))
              "d(sin(2x)*cos(x^2))")
