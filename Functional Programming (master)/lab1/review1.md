% Лабораторная работа № 1. Основы языков семейства Лисп
% 18 сентября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Знакомство с языками семейства Лисп на примере языка программирования Scheme,
освоение основных управляющих конструкций, системы типов и
принципов написания программ.

# Индивидуальный вариант
Определите следующие процедуры для обработки списков:

- Процедуру (my-range a b d), возвращающую список чисел в интервале [a, b) с шагом d.
  Параметр d — не
  обязательный, если он отсутствует — подразумевается 1.
- Процедуру my-flatten, раскрывающую вложенные списки.
- Предикат (my-element? x xs), проверяющий наличие элемента x в списке xs.
  Рекомендация: для проверки
  равенства элементов используйте встроенный предикат equal?.
- Предикат (my-filter pred? xs), возвращающий список только тех элементов списка xs,
  которые удовлетворяют предикату pred?.
- Процедуру (my-fold-left op xs) для левоассоциативной свертки списка xs с
  помощью оператора (процедуры двух аргументов) op.
- Процедуру (my-fold-right op xs) для правоассоциативной свертки списка xs с помощью оператора
  (процедуры двух аргументов) op.
- Процедуры rac, rdc и snoc, работающие аналогично car, cdr и cons, но с концом списка.
  Решить без использования обращения списка.

Реализуйте библиотеку процедур для работы со множествами (для ханения множеств используйте
списки):

- Процедуру (list->set xs), преобразующую список xs в множество.
- Предикат (set? xs), проверяющий, является ли список xs множеством.
- Процедуру (union xs ys), возвращающую объединение множеств xs и ys.
- Процедуру (intersection xs ys), возвращающую пересечение множеств xs и ys.
- Процедуру (difference xs ys), возвращающую разность множеств xs и ys.
- Процедуру (symmetric-difference xs ys), возвращающую симметричную разность множеств xs и ys.
- Предикат (set-eq? xs ys), проверяющий множества xs и ys на равенство друг другу.
- Процедуры union и intersection обобщить на переменное количество параметров: union должна принимать
  0 и более параметров (без параметров возвращает пустое множество), intersection — 1 и более параметров

Реализуйте библиотеку процедур для работы со строками. Реализуйте следующие процедуры:

- Процедуры string-trim-left, string-trim-right и string-trim, удаляющие все пробельные
  символы в начале, конце и с обеих сторон строки соответственно.
- Предикаты (string-prefix? a b), (string-suffix? a b) и (string-infix? a b), соответственно,
  проверяющие, является ли строка a: началом строки b, окончанием строки b или строка a где-либо встречается в строке b.
- Процедуру (string-split str sep), возвращающую список подстрок строки str, разделённых
  в строке str разделителями sep, где sep — непустая строка. Т.е. процедура (string-split str sep) должна
  разбивать строку на подстроки по строке-разделителю sep.
- Процедура replace выполняющая замену подстроки в строке:



# Реализация

```scheme
#lang racket

#lang racket

(display "\n-----LIST TESTS-----\n")

(define (check-equal? actual expected)
  (if (equal? actual expected)
      (displayln "OK")
      (begin
        (display "FAIL: got ") (display actual)
        (display ", expected ") (displayln expected))))

(define (my-range a b . maybe-d)
  (let ((d (if (null? maybe-d) 1 (car maybe-d))))
    (cond
      ((and (> d 0) (>= a b)) '())
      ((and (< d 0) (<= a b)) '())
      ((= d 0) (error "my-range: шаг не может быть равен 0"))
      (else (cons a (my-range (+ a d) b d))))))


(define (my-flatten xs)
  (cond ((null? xs) '())
        ((pair? (car xs))
         (append (my-flatten (car xs)) (my-flatten (cdr xs))))
        (else
         (cons (car xs) (my-flatten (cdr xs))))))


(define (my-element? x xs)
  (cond ((null? xs) #f)
        ((equal? x (car xs)) #t)
        (else (my-element? x (cdr xs)))))


(define (my-filter pred? xs)
  (cond ((null? xs) '())
        ((pred? (car xs)) (cons (car xs) (my-filter pred? (cdr xs))))
        (else (my-filter pred? (cdr xs)))))

(define (my-fold-left op xs)
    (if (null? xs)
      (error "my-fold-right: пустой список")
      (if (null? (cdr xs))
          (car xs)
          (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs))))))

(define (my-fold-right op xs)
  (if (null? xs)
      (error "my-fold-right: пустой список")
      (if (null? (cdr xs))
          (car xs)
          (op (car xs) (my-fold-right op (cdr xs))))))

(define (rac xs)
  (if (null? xs)
      (error "my-fold-right: пустой список")
      (if (null? (cdr xs))
          (car xs)
          (rac (cdr xs)))))

(define (rdc xs)
  (if (null? xs)
      (error "my-fold-right: пустой список")
      (if (null? (cdr xs))
          '()
          (cons (car xs) (rdc (cdr xs))))))

(define (snoc xs x)
  (if (null? xs)
      (list x)
      (cons (car xs) (snoc (cdr xs) x))))




;; my-range
(display "(my-range 1 10 2) -> (1 3 5 7 9) ")
(check-equal? (my-range 1 10 2) '(1 3 5 7 9))

(display "(my-range 0 11 3) -> (0 3 6 9) ")
(check-equal? (my-range 0 11 3) '(0 3 6 9))

(display "(my-range 0 11 1) -> (0 1 2 3 4 5 6 7 8 9 10) ")
(check-equal? (my-range 0 11 1) '(0 1 2 3 4 5 6 7 8 9 10))

(display "(my-range 0 11) -> (0 1 2 3 4 5 6 7 8 9 10) ")
(check-equal? (my-range 0 11) '(0 1 2 3 4 5 6 7 8 9 10))

;; my-flatten
(display "(my-flatten '((1) 2 (3 (4 5)) 6)) -> (1 2 3 4 5 6) ")
(check-equal? (my-flatten '((1) 2 (3 (4 5)) 6)) '(1 2 3 4 5 6))

;; my-element?
(display "(my-element? 1 '(3 2 1)) -> #t ")
(check-equal? (my-element? 1 '(3 2 1)) #t)

(display "(my-element? 4 '(3 2 1)) -> #f ")
(check-equal? (my-element? 4 '(3 2 1)) #f)

;; my-filter
(display "(my-filter odd? (my-range 0 10 1)) -> (1 3 5 7 9) ")
(check-equal? (my-filter odd? (my-range 0 10 1)) '(1 3 5 7 9))

(display "(my-filter divisible-by-3 (my-range 0 13 1)) -> (0 3 6 9 12) ")
(check-equal? (my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
              '(0 3 6 9 12))

;; fold
(display "(my-fold-left quotient '(16 2 2 2 2)) -> 1 ")
(check-equal? (my-fold-left quotient '(16 2 2 2 2)) 1)

(display "(my-fold-right expt '(2 3 4)) -> 2417851639229258349412352 ")
(check-equal? (my-fold-right expt '(2 3 4)) 2417851639229258349412352)

;; rac, rdc, snoc
(display "(rac '(a b c d)) -> d ")
(check-equal? (rac '(a b c d)) 'd)

(display "(rdc '(a b c d)) -> (a b c) ")
(check-equal? (rdc '(a b c d)) '(a b c))
 
(display "(snoc '(a b c) 'd) -> (a b c d) ")
(check-equal? (snoc '(a b c) 'd) '(a b c d))


;;-------------------------------------------------------------------------------


(define (elem? x xs)
  (cond ((null? xs) #f)
        ((equal? x (car xs)) #t)
        (else (elem? x (cdr xs)))))

(define (list->set xs)
  (if (null? xs)
      '()
      (if (elem? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))

(define (set? xs)
  (cond ((null? xs) #t)
        ((elem? (car xs) (cdr xs)) #f)
        (else (set? (cdr xs)))))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (elem? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

(define (difference xs ys)
  (if (null? xs)
      '()
      (if (elem? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))

(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

(define (set-eq? xs ys)
  (and (subset? xs ys) (subset? ys xs)))

(define (subset? xs ys)
  (cond ((null? xs) #t)
        ((elem? (car xs) ys) (subset? (cdr xs) ys))
        (else #f)))

(define (union-many . sets)
  (if (null? sets)
      '()
      (my-fold-left union sets)))

(define (intersection-many first . rest)
  (if (null? rest)
      first
      (my-fold-left intersection (cons first rest))))

(display "\n-----SET TESTS-----\n")

;; list->set
(display "(list->set '(1 1 2 3)) -> (1 2 3)")
(check-equal? (list->set '(1 1 2 3)) '(1 2 3))

(display "(list->set '()) -> ") 
(check-equal? (list->set '()) '())

;; set?
(display "(set? '(1 2 3)) -> #t ")
(check-equal? (set? '(1 2 3)) #t)

(display "(set? '(1 2 3 3)) -> #f ")
(check-equal? (set? '(1 2 3 3)) #f)

(display "(set? '()) -> #t ")
(check-equal? (set? '()) #t)

;; union
(display "(union '(1 2 3) '(2 3 4)) -> (1 2 3 4) ")
(check-equal? (union '(1 2 3) '(2 3 4)) '(1 2 3 4))

(display "(union '() '(1 2)) -> (1 2) ") 
(check-equal? (union '() '(1 2)) '(1 2))

(display "(union '(1 2) '()) -> (1 2) ") 
(check-equal? (union '(1 2) '()) '(1 2))

(display "(union '() '()) -> () ") 
(check-equal? (union '() '()) '())

;; intersection
(display "(intersection '(1 2 3) '(2 3 4)) -> (2 3) ")
(check-equal? (intersection '(1 2 3) '(2 3 4)) '(2 3))

(display "(intersection '() '(1 2)) -> () ") 
(check-equal? (intersection '() '(1 2)) '())

(display "(intersection '(1 2) '()) -> ()") 
(check-equal? (intersection '(1 2) '()) '())

;; difference
(display "(difference '(1 2 3 4 5) '(2 3)) -> (1 4 5) ")
(check-equal? (difference '(1 2 3 4 5) '(2 3)) '(1 4 5))

(display "(difference '(2 3) '(3 4 5)) -> (2) ")
(check-equal? (difference '(2 3) '(3 4 5)) '(2))

(display "(difference '() '(1 2)) -> () ") 
(check-equal? (difference '() '(1 2)) '())

(display "(difference '(1 2) '()) -> (1 2) ") 
(check-equal? (difference '(1 2) '()) '(1 2))

;; symmetric-difference
(display "(symmetric-difference '(1 2 3 4) '(3 4 5 6)) -> (1 2 5 6) ")
(check-equal? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(1 2 5 6))

(display "(symmetric-difference '() '(1 2)) -> (1 2)") 
(check-equal? (symmetric-difference '() '(1 2)) '(1 2))

(display "(symmetric-difference '(1 2) '()) -> (1 2)") 
(check-equal? (symmetric-difference '(1 2) '()) '(1 2))

(display "(symmetric-difference '() '()) -> () ") 
(check-equal? (symmetric-difference '() '()) '())

;; set-eq?
(display "(set-eq? '(1 2 3) '(3 2 1)) -> #t ")
(check-equal? (set-eq? '(1 2 3) '(3 2 1)) #t)

(display "(set-eq? '(1 2) '(1 3)) -> #f ")
(check-equal? (set-eq? '(1 2) '(1 3)) #f)

(display "(set-eq? (list->set '(1 1 2 3)) '(1 2 3)) -> #t ")
(check-equal? (set-eq? (list->set '(1 1 2 3)) '(1 2 3)) #t)

(display "(set-eq? (union '(1 2 3) '(2 3 4)) '(4 3 2 1)) -> #t ")
(check-equal? (set-eq? (union '(1 2 3) '(2 3 4)) '(4 3 2 1)) #t)

(display "(set-eq? (intersection '(1 2 3) '(2 3 4)) '(2 3)) -> #t ")
(check-equal? (set-eq? (intersection '(1 2 3) '(2 3 4)) '(2 3)) #t)

(display "(set-eq? (difference '(1 2 3 4 5) '(2 3)) '(1 4 5)) -> #t ")
(check-equal? (set-eq? (difference '(1 2 3 4 5) '(2 3)) '(1 4 5)) #t)

(display "(set-eq? (difference '(2 3) '(3 4 5)) '(2)) -> #t ")
(check-equal? (set-eq? (difference '(2 3) '(3 4 5)) '(2)) #t)

(display "(set-eq? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(1 2 5 6)) -> #t ")
(check-equal? (set-eq? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(1 2 5 6)) #t)

;; union с произвольным числом множеств
(display "(set-eq? (union-many '(a b c) '(b d a) '(p q b)) '(a b c d p q)) -> #t ")
(check-equal? (set-eq? (union-many '(a b c) '(b d a) '(p q b)) '(a b c d p q)) #t)

(display "(union-many) -> () ")
(check-equal? (union-many) '())

(display "(union-many '(1 2) '(2 3) '(3 4)) ->  #t ")
(check-equal? (set-eq? (union-many '(1 2) '(2 3) '(3 4)) '(1 2 3 4)) #t)

;; intersection с произвольным числом множеств
(display "(intersection-many '(a b c) '(b d a) '(p q b)) -> (b) ")
(check-equal? (intersection-many '(a b c) '(b d a) '(p q b)) '(b))

(display "(intersection-many '(1 2 3) '(2 3 4) '(3 4 5)) ->  (3) ")
(check-equal? (intersection-many '(1 2 3) '(2 3 4) '(3 4 5)) '(3))

(display "(intersection-many '(1 2 3)) -> (1 2 3) ")
(check-equal? (intersection-many '(1 2 3)) '(1 2 3))

(display "(intersection-many '(1 2 3) '() ) -> () ")
(check-equal? (intersection-many '(1 2 3) '() ) '())


;;-------------------------------------------------------------------------------


(define (string-trim-left str)
  (let loop ((i 0))
    (if (or (= i (string-length str)) (not (char-whitespace? (string-ref str i))))
        (substring str i (string-length str))
        (loop (+ i 1)))))

(define (string-trim-right str)
  (let loop ((i (- (string-length str) 1)))
    (if (or (< i 0) (not (char-whitespace? (string-ref str i))))
        (substring str 0 (+ i 1))
        (loop (- i 1)))))

(define (string-trim str)
  (string-trim-left (string-trim-right str)))

(define (string-prefix? a b)
  (and (<= (string-length a) (string-length b))
       (string=? a (substring b 0 (string-length a)))))

(define (string-suffix? a b)
  (and (<= (string-length a) (string-length b))
       (string=? a (substring b (- (string-length b) (string-length a))))))

(define (string-infix? a b)
    (let loop ((i 0))
      (cond ((> i (- (string-length b) (string-length a))) #f)
            ((string=? a (substring b i (+ i (string-length a)))) #t)
            (else (loop (+ i 1))))))

(define (string-split str sep)
    (let loop ((i 0) (start 0) (res '()))
      (if (>= i (- (string-length str) (string-length sep) -1))
          (if (and (>= (- start 1) 0) (string=? (substring str (- start 1)) sep))
              (reverse  res)
              (reverse (cons (substring str start) res)))
          (if (string=? (substring str i (+ i (string-length sep))) sep)
              (loop (+ i (string-length sep)) (+ i (string-length sep)) (cons (substring str start i) res))
              (loop (+ i 1) start res)))))
                                                           
 (define (replace str old new)
    (let loop ((i 0) (res ""))
      (if (>= i (string-length str))
          res
          (if (and (<= (+ i (string-length old)) (string-length str))
                   (string=? old (substring str i (+ i (string-length old)))))
              (loop (+ i (string-length old)) (string-append res new))
              (loop (+ i 1) (string-append res (string (string-ref str i))))))))



(display "\n-----STRING TESTS-----\n")

;; string-trim-left
(display "(string-trim-left \"\t\tabc def\") -> \"abc def\" ") 
(check-equal? (string-trim-left "\t\tabc def") "abc def")

;; string-trim-right
(display "(string-trim-right \"abc def\t\") -> \"abc def\" ") 
(check-equal? (string-trim-right "abc def\t") "abc def")

;; string-trim
(display "(string-trim \"\t abc def \n\") -> \"abc def\" ") 
(check-equal? (string-trim "\t abc def \n") "abc def")

;; string-prefix?
(display "(string-prefix? \"abc\" \"abcdef\") -> #t ") 
(check-equal? (string-prefix? "abc" "abcdef") #t)

(display "(string-prefix? \"bcd\" \"abcdef\") -> #f ") 
(check-equal? (string-prefix? "bcd" "abcdef") #f)

;; string-suffix?
(display "(string-suffix? \"def\" \"abcdef\") -> #t ") 
(check-equal? (string-suffix? "def" "abcdef") #t)

(display "(string-suffix? \"bcd\" \"abcdef\") -> #f ") 
(check-equal? (string-suffix? "bcd" "abcdef") #f)

;; string-infix?
(display "(string-infix? \"def\" \"abcdefgh\") -> #t ") 
(check-equal? (string-infix? "def" "abcdefgh") #t)

(display "(string-infix? \"abc\" \"abcdefgh\") -> #t ") 
(check-equal? (string-infix? "abc" "abcdefgh") #t)

(display "(string-infix? \"fgh\" \"abcdefgh\") -> #t ") 
(check-equal? (string-infix? "fgh" "abcdefgh") #t)

(display "(string-infix? \"ijk\" \"abcdefgh\") -> #f ") 
(check-equal? (string-infix? "ijk" "abcdefgh") #f)

;; string-split
(display "(string-split \"x;y;z;\" \";\") -> '(\"x\" \"y\" \"z\") ") 
(check-equal? (string-split "x;y;z;" ";") '("x" "y" "z"))

(display "(string-split \"x;y;z\" \";\") -> '(\"x\" \"y\" \"z\") ") 
(check-equal? (string-split "x;y;z" ";") '("x" "y" "z"))

(display "(string-split \"x-->y-->z\" \"-->\") -> '(\"x\" \"y\" \"z\") ") 
(check-equal? (string-split "x-->y-->z" "-->") '("x" "y" "z"))

;; replace
(display "(replace \"кот любит антрекот\" \"кот\" \"пёс\") -> \"пёс любит антрепёс\" ") 
(check-equal? (replace "кот любит антрекот" "кот" "пёс") "пёс любит антрепёс")

(display "(replace \"кот любит антрекот\" \"кот\" \"пёс\") -> \"тест\" ") 
(check-equal? (replace "кот любит антрекот" "кот" "пёс") "пёс любит антрепёс")

;; corner cases
(display "(string-trim \"\") -> \"\" ") 
(check-equal? (string-trim "") "")

(display "(string-split \"\" \";\") -> '(\"\") ") 
(check-equal? (string-split "" ";") '(""))

(display "(string-split \"abc\" \";\") -> '(\"abc\") ") 
(check-equal? (string-split "abc" ";") '("abc"))

(display "(string-prefix? \"\" \"abc\") -> #t ") 
(check-equal? (string-prefix? "" "abc") #t)

(display "(string-suffix? \"\" \"abc\") -> #t ") 
(check-equal? (string-suffix? "" "abc") #t)

(display "(string-infix? \"\" \"abc\") -> #t ") 
(check-equal? (string-infix? "" "abc") #t)

```

# Тестирование

```
Welcome to DrRacket, version 8.17 [cs].
Language: racket, with debugging; memory limit: 128 MB.

-----LIST TESTS-----
(my-range 1 10 2) -> (1 3 5 7 9) OK
(my-range 0 11 3) -> (0 3 6 9) OK
(my-range 0 11 1) -> (0 1 2 3 4 5 6 7 8 9 10) OK
(my-range 0 11) -> (0 1 2 3 4 5 6 7 8 9 10) OK
(my-flatten '((1) 2 (3 (4 5)) 6)) -> (1 2 3 4 5 6) OK
(my-element? 1 '(3 2 1)) -> #t OK
(my-element? 4 '(3 2 1)) -> #f OK
(my-filter odd? (my-range 0 10 1)) -> (1 3 5 7 9) OK
(my-filter divisible-by-3 (my-range 0 13 1)) -> (0 3 6 9 12) OK
(my-fold-left quotient '(16 2 2 2 2)) -> 1 OK
(my-fold-right expt '(2 3 4)) -> 2417851639229258349412352 OK
(rac '(a b c d)) -> d OK
(rdc '(a b c d)) -> (a b c) OK
(snoc '(a b c) 'd) -> (a b c d) OK

-----SET TESTS-----
(list->set '(1 1 2 3)) -> (1 2 3)OK
(list->set '()) -> OK
(set? '(1 2 3)) -> #t OK
(set? '(1 2 3 3)) -> #f OK
(set? '()) -> #t OK
(union '(1 2 3) '(2 3 4)) -> (1 2 3 4) OK
(union '() '(1 2)) -> (1 2) OK
(union '(1 2) '()) -> (1 2) OK
(union '() '()) -> () OK
(intersection '(1 2 3) '(2 3 4)) -> (2 3) OK
(intersection '() '(1 2)) -> () OK
(intersection '(1 2) '()) -> ()OK
(difference '(1 2 3 4 5) '(2 3)) -> (1 4 5) OK
(difference '(2 3) '(3 4 5)) -> (2) OK
(difference '() '(1 2)) -> () OK
(difference '(1 2) '()) -> (1 2) OK
(symmetric-difference '(1 2 3 4) '(3 4 5 6)) -> (1 2 5 6) OK
(symmetric-difference '() '(1 2)) -> (1 2)OK
(symmetric-difference '(1 2) '()) -> (1 2)OK
(symmetric-difference '() '()) -> () OK
(set-eq? '(1 2 3) '(3 2 1)) -> #t OK
(set-eq? '(1 2) '(1 3)) -> #f OK
(set-eq? (list->set '(1 1 2 3)) '(1 2 3)) -> #t OK
(set-eq? (union '(1 2 3) '(2 3 4)) '(4 3 2 1)) -> #t OK
(set-eq? (intersection '(1 2 3) '(2 3 4)) '(2 3)) -> #t OK
(set-eq? (difference '(1 2 3 4 5) '(2 3)) '(1 4 5)) -> #t OK
(set-eq? (difference '(2 3) '(3 4 5)) '(2)) -> #t OK
(set-eq? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(1 2 5 6)) -> #t OK
(set-eq? (union-many '(a b c) '(b d a) '(p q b)) '(a b c d p q)) -> #t OK
(union-many) -> () OK
(union-many '(1 2) '(2 3) '(3 4)) ->  #t OK
(intersection-many '(a b c) '(b d a) '(p q b)) -> (b) OK
(intersection-many '(1 2 3) '(2 3 4) '(3 4 5)) ->  (3) OK
(intersection-many '(1 2 3)) -> (1 2 3) OK
(intersection-many '(1 2 3) '() ) -> () OK

-----STRING TESTS-----
(string-trim-left "		abc def") -> "abc def" OK
(string-trim-right "abc def	") -> "abc def" OK
(string-trim "	 abc def 
") -> "abc def" OK
(string-prefix? "abc" "abcdef") -> #t OK
(string-prefix? "bcd" "abcdef") -> #f OK
(string-suffix? "def" "abcdef") -> #t OK
(string-suffix? "bcd" "abcdef") -> #f OK
(string-infix? "def" "abcdefgh") -> #t OK
(string-infix? "abc" "abcdefgh") -> #t OK
(string-infix? "fgh" "abcdefgh") -> #t OK
(string-infix? "ijk" "abcdefgh") -> #f OK
(string-split "x;y;z;" ";") -> '("x" "y" "z") OK
(string-split "x;y;z" ";") -> '("x" "y" "z") OK
(string-split "x-->y-->z" "-->") -> '("x" "y" "z") OK
(replace "кот любит антрекот" "кот" "пёс") -> "пёс любит антрепёс" OK
(replace "кот любит антрекот" "кот" "пёс") -> "тест" OK
(string-trim "") -> "" OK
(string-split "" ";") -> '("") OK
(string-split "abc" ";") -> '("abc") OK
(string-prefix? "" "abc") -> #t OK
(string-suffix? "" "abc") -> #t OK
(string-infix? "" "abc") -> #t OK
```

# Вывод
В данной лабораторной работе мною было разблокировано чудесное воспоминание
по работе с языком Scheme. Это очень интересно, потому что, когда на первом курсе
ты приходишь с нулевыми знаниями, ты пугаешься языка Scheme и не понимаешь,
что к чему. Сейчас же есть ясность написания программ, и ты получаешь удовольствие
от красивых рекурсивных конструкций, которые дарит функциональное программирование.
Помимо этого и ностальгическая нотка также присутствует.

Также помимо основного задания решил немного автоматизировать проверку ответов,
с помощью аналога функции assert, мне пониравилась такая идея, хотя, больше, мне
понравилось осознание того, что на первом курсе мне бы не пришло это в голову,
ведь я был ещё юнцом в мире программирования. Чудесно!