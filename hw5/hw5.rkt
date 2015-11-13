#!r6rs
(import (rnrs eval (6))
        (rnrs base (6))
        (rnrs io simple (6)))

(define returnlist4
  (lambda(a b c d) 
    (cons a (cons b (cons c (cons d '()))))
))

(define returnlist1
  (lambda(a)
    (cons a '())
))

(define compare-expr-constant
  (lambda (x y)
    (if (equal? x y) 
      x 
      (if (and (equal? x #t) (equal? y #f))
        'TCP
        (if (and (equal? x #f) (equal? y #t))
          '(not TCP)
          (returnlist4 'if 'TCP x y))))
))

(define (compare-expr-quote x y)
  (compare-expr-constant x y)
)

; If the lambdas has the same formal, we compare their body as lists; if not functions are treated as completely different
(define (compare-expr-lambda x y)
  (if (equal? (car (cdr x)) (car (cdr y)))
    (compare-expr-list x y)
    (compare-expr-constant x y))
)

(define (let-binding-same-variables x y)
  (if (and (equal? x '()) (equal? y '()))
    #t
    (if (equal? (car (car x)) (car (car y)))
      (let-binding-same-variables (cdr x) (cdr y))
      #f))
)

; If the lets are binding the same variables, we compare their body as lists; if not functions are treated as completely different
(define (compare-expr-let x y)
  (if (let-binding-same-variables (car (cdr x)) (car (cdr y)))
    (compare-expr-list x y)
    (compare-expr-constant x y))
)

; This will only compare when both are (if x y z) or (if x y), and won't compare (if x y z) with (if x y)
; Turns out that both side 'if does not require special handling, other than what's already in compare-expr-list
;(define (compare-expr-if x y)
;  (compare-expr-list x y)
;)

; We are calling different functions on both sides, if one side contains a 'if, 'quote, 'let or 'lambda, we should treat both lists as completely different
(define (builtin-function-on-either-side x y)
  (if (or (equal? (car x) 'if) (equal? (car y) 'if))
    #t
    (if (or (equal? (car x) 'quote) (equal? (car y) 'quote))
      #t
      (if (or (equal? (car x) 'lambda) (equal? (car y) 'lambda))
        #t
        (if (or (equal? (car x) 'let) (equal? (car y) 'let))
          #t
          #f))))
)

(define (compare-expr x y)
  (if (and (list? x) (list? y))
    (if (equal? (length x) (length y))
      (if (equal? (car x) (car y))
        (case (car x)
          ('quote (compare-expr-quote x y))
          ;['if (compare-expr-if x y)]
          ('lambda (compare-expr-lambda x y))
          ('let (compare-expr-let x y))
          (else (compare-expr-list x y)))
        (if (builtin-function-on-either-side x y)
          (compare-expr-constant x y)
          (compare-expr-list x y)))
      ; According to test cases, if x y z, and if x y can be treated as completely different
      (compare-expr-constant x y))
    (compare-expr-constant x y))
)

(define compare-expr-list
  (lambda (x y)
    (if (equal? x '()) 
      '() 
      (if (equal? y '()) 
        '() 
        (cons (compare-expr (car x) (car y)) (compare-expr-list (cdr x) (cdr y)))))
))

(define test-x
  '(cons
    (+
      ; tests lambda with same formals
      ((lambda (a b c)
        ; tests if/3 with same guard but different then-dos and else-dos
        (if a b c))
          ; tests boolean constants
          #t
          ; tests let bindings with same formals but different values
          (if #t (let ((d 13)) (+ d 15)) 27)
          ; tests ordinary function calls and quoted lists
          (car (cons 21 (cons 3 '(45 78)))))
      ; tests one side with if and the other side with an arbitrary function call
      (let ((a #t)) (if a 34)))
    (cons
      (if #f
        ; tests lambdas with different formals
        ((lambda (x y) (+ y x)) 1 2)
        ; tests let bindings with different formals
        (let ((x 2) (y 3)) (+ x y)))
      (cons
        ; tests quoted lists
        '(if #f 2 3)
        ; tests if/2 vs if/3
        (if #t '(23) '(14)))))
)

(define test-y
  '(cons
    (+
      ((lambda (a b c)
        (if a c b))
          #f
          (if #t (let ((d 14)) (+ d 15)) 27)
          (car (cons 2 (cons 3 '(2 3)))))
      (let ((a 40)) (- a 34)))
    (cons
      (if #t
        ((lambda (y x) (+ x y)) 1 2)
        (let ((y 3) (x 2)) (+ x y)))
      (cons
        (if #f 2 3)
        (if #t '(23)))))
)

(define (build-evaluation-list x bool)
  (if bool
    (cons 'let (cons '((TCP #t)) (cons x '())))
    (cons 'let (cons '((TCP #f)) (cons x '()))))
)

; Note: The evaluation may throw exceptions, which are not caught in this case,
; since the assignment states that "Your prototype need not check that its inputs are valid"
(define (evaluate x)
  (eval x (environment '(rnrs)))
)

(define (test-compare-expr x y)
  (let ((result (compare-expr x y)))
    (if (and
      (equal?
        (evaluate (build-evaluation-list result #t))
        (evaluate x))
      (equal?
        (evaluate (build-evaluation-list result #f))
        (evaluate y)))
      #t
      #f))
)

(display (test-compare-expr test-x test-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The compare-expr of test-x and test-y gives:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (equal? (compare-expr test-x test-y)

; '(cons
;   (+
;    ((lambda (a b c) (if a (if TCP b c) (if TCP c b)))
;     TCP
;     (if #t (let ((d (if TCP 13 14))) (+ d 15)) 27)
;     (car (cons (if TCP 21 2) (cons 3 (if TCP '(45 78) '(2 3))))))
;    (let ((a (if TCP #t 40))) (if TCP (if a 34) (- a 34))))
;   (cons
;    (if (not TCP)
;      ((if TCP (lambda (x y) (+ y x)) (lambda (y x) (+ x y))) 1 2)
;      (if TCP (let ((x 2) (y 3)) (+ x y)) (let ((y 3) (x 2)) (+ x y))))
;    (cons
;     (if TCP '(if #f 2 3) (if #f 2 3))
;     (if TCP (if #t '(23) '(14)) (if #t '(23))))))

; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unit test cases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (equal?
; (compare-expr 12 20) '(if TCP 12 20))

; (equal?
; (compare-expr 12 12) 12)
; (equal?
; (compare-expr #f #t) '(not TCP))
; (equal?
; (compare-expr #t #f) 'TCP)
; (equal?
; (compare-expr #f #f) #f)

; (equal?
; (compare-expr '(12 20) '(12 12)) '(12 (if TCP 20 12)))
; (equal?
; (compare-expr '(12 20 30) '(12 12)) '(if TCP (12 20 30) (12 12)))
; (equal?
; (compare-expr '(12 (20 40) 30) '(12 (20 30) 12)) '(12 (20 (if TCP 40 30)) (if TCP 30 12)))

; (equal?
; (compare-expr '(quoth (a b)) '(quoth (a c))) '(quoth (a (if TCP b c))))
; (equal?
; (compare-expr '(12 '(20 40) 30) '(12 '(20 30) 12)) '(12 (if TCP '(20 40) '(20 30)) (if TCP 30 12)))
; (equal?
; (compare-expr '(quote (if x y z)) '(quote (if x z y))) '(if TCP '(if x y z) '(if x z y)))

; (equal?
; (compare-expr '(if x y z) '(if x z z)) '(if x (if TCP y z) z))
; (equal?
; (compare-expr '(if x y z) '(if x z y)) '(if x (if TCP y z) (if TCP z y)))
; (equal?
; (compare-expr '(if y y z) '(if x z z)) '(if (if TCP y x) (if TCP y z) z))
; (equal?
; (compare-expr '(if x #t) '(if x #f)) '(if x TCP))

; (equal?
; (compare-expr '(if x y z) 
;               '(g x y z))
;               '(if TCP (if x y z) (g x y z)))
; (equal?
; (compare-expr '(quote (if x y z)) 
;               '(g (if x y z)))
;               '(if TCP '(if x y z) (g (if x y z))))
; (equal?
; (compare-expr '(lambda x x z) 
;               '(g x x z))
;               '(if TCP (lambda x x z) (g x x z)))
; (equal?
; (compare-expr '(quote (if x y z)) 
;               '(g (if x y z) 20))
;               '(if TCP '(if x y z) (g (if x y z) 20)))

; (equal?
; (compare-expr '((lambda (a) (f a)) 1) 
;               '((lambda (a) (g a)) 2))
;               '((lambda (a) ((if TCP f g) a)) (if TCP 1 2)))
; (equal?
; (compare-expr '((lambda (a b) (f a b)) 1 2) 
;               '((lambda (a c) (f c a)) 1 2))
;               '((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) 1 2))
; (equal?
; (compare-expr '((lambda (a b) (f a b)) 1 2) 
;               '((lambda (a c) (f c a)) 2 1))
;               '((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) (if TCP 1 2) (if TCP 2 1)))

; (equal?
; (compare-expr '(cons a b) 
;               '(cons a c))
;               '(cons a (if TCP b c)))
; (equal?
; (compare-expr '(cons (cons a b) (cons b c)) 
;               '(cons (cons a c) (cons a c)))
;               '(cons (cons a (if TCP b c)) (cons (if TCP b a) c)))
; (equal?
; (compare-expr '(cons a b) 
;               '(list a b))
;               '((if TCP cons list) a b))

; (equal?
; (compare-expr '(let ((a 1)) (f a)) 
;               '(let ((a 2)) (g a)))
;               '(let ((a (if TCP 1 2))) ((if TCP f g) a)))

; (equal?
; (compare-expr '(+ #f (let ((a 1) (b 2)) (f a b)))
;               '(+ #t (let ((a 1) (c 2)) (f a c))))
;               '(+ (not TCP) (if TCP (let ((a 1) (b 2)) (f a b)) (let ((a 1) (c 2)) (f a c)))))

