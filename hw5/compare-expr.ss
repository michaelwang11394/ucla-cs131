; #!r6rs
; (import (rnrs eval (6))
;         (rnrs base (6)))

; How to run (with !r6rs):
;   To test if/3 vs if/2, toggle the last commented line in test-y definition, and use the form with 'environment' in the evaluation function
;   Loading with "racket < compare-expr.ss" would cause problem; 
;   While loading with (load "compare-expr.ss") is fine, but does not output anything even with display function added;
;   Loading with the GUI DrRacket is fine, and adding display would display properly; (Recommended way to test, if applicable)
;   To add print debugging, include (rnrs io simple (6));
;
; Or alternatively, 
;   Leave the import and #!r6rs as commented, and run unit test cases or evaluation tests
;   As commented below, current evaluation test comments out the if/2 vs if/3 comparison, 
;   and current evaluation function comments out the 'environment' in eval, thus this should work without r6rs as is

; Constructs the list with 'if 'TCP 'execution-1 'execution-2
(define returnlist4
  (lambda(a b c d) 
    (cons a (cons b (cons c (cons d '()))))
))

; Comparison function for constant items, 
; handles numeric and boolean constants, quoted lists, and all things that should be treated as completely different
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

; Comparison function for lists of the same length, 
; this is used when we need to handle each element in the list recursively, instead of just treating the whole list as different
(define compare-expr-list
  (lambda (x y)
    (if (equal? x '()) 
      '() 
      (if (equal? y '()) 
        '() 
        (cons (compare-expr (car x) (car y)) (compare-expr-list (cdr x) (cdr y)))))
))

; Wrapper function for quote handling
(define (compare-expr-quote x y)
  (compare-expr-constant x y)
)

; Lambda handling
; If the lambdas has the same formal, we compare their body as lists; if not lambdas are treated as completely different
(define (compare-expr-lambda x y)
  (if (equal? (car (cdr x)) (car (cdr y)))
    (compare-expr-list x y)
    (compare-expr-constant x y))
)

; Check if let binds the same ordered list of variables by tail recursion, we don't care about the values in this case
(define (let-binding-same-variables x y)
  (if (and (equal? x '()) (equal? y '()))
    #t
    (if (equal? (car (car x)) (car (car y)))
      (let-binding-same-variables (cdr x) (cdr y))
      #f))
)

; Let handling
; If the lets are binding the same variables, we compare their body as lists; if not functions are treated as completely different
(define (compare-expr-let x y)
  (if (let-binding-same-variables (car (cdr x)) (car (cdr y)))
    (compare-expr-list x y)
    (compare-expr-constant x y))
)

; This will only compare when both are (if x y z) or (if x y), and won't compare (if x y z) with (if x y) because they are of different lengths
; Turns out that both side 'if does not require special handling, other than what's already in compare-expr-list
;(define (compare-expr-if x y)
;  (compare-expr-list x y)
;)

; Check if at least one side's calling a built-in function, when different functions are called on both sides
; Built-in functions: 'if, 'quote, 'let or 'lambda; Upon returning true, we should treat both lists as completely different
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

; Body function, check two lists by tail recursion; Logic:
; For each element, 
;   if both sides are list, check if lengths match; 
;      if so check if both sides are calling the same functions 
;          if so check if that function's one of the built-in functions ('let, 'quote, 'lambda)
;              if so, handle it accordingly
;              if not, handle as ordinary list function call
;          if not check if at least one side's calling a built-in function
;              if so, treat both as completely different
;              if not, handle as ordinary list function call
;      if not match as constants
;   if at least one side's a literal, match as constants
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

; Whole test case, test purposes commented below
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
      ; tests one side with built-in function and the other side with an arbitrary function call
      ; this uses if/2 by default, which is not supported as is; recommend toggle comments between the following two lines if playing with r6rs
      ;(let ((a #t)) (if a 34)))
      (let ((a #t)) (let ((a 34)) a)))
    (cons
      (if #f
        ; tests lambdas with different formals
        ; different formal orders treated as completely different, per my question on piazza
        ((lambda (x y) (+ y x)) 1 2)
        ; tests let bindings with different formals
        (let ((x 2) (y 3)) (+ x y)))
      (cons
        ; tests quoted lists
        '(if #f 2 3)
        ; tests if/2 vs if/3; 
        ; Toggle comments in test-y when if/2 is supported, for example, in r6rs
        (if #t '(23) '(14)))))
)

(define test-y
  '(cons
    (+
      ((lambda (a b c)
        (if a c b))
          #f
          (if #t (let ((d 14)) (+ d 15)) 27)
          (car (cons 2 (cons 3 '(45 3)))))
      (let ((a 40)) (- a 34)))
    (cons
      (if #t
        ((lambda (y x) (+ x y)) 1 2)
        (let ((y 3) (x 2)) (+ x y)))
      (cons
        (if #f 2 3)
        ;(if #t '(23)))))
        (if #t '(23) '()))))
)

; Create evaluatable list with TCP let bindings
(define (build-evaluation-list x bool)
  (if bool
    (cons 'let (cons '((TCP #t)) (cons x '())))
    (cons 'let (cons '((TCP #f)) (cons x '()))))
)

; Note: The evaluation may throw exceptions, which are not caught in this case,
;   since the assignment states that "Your prototype need not check that its inputs are valid"
(define (evaluate x)
  ; Use the form with 'environment' when r6rs is included
  (eval x)
  ;(eval x (environment '(rnrs)))
)

; Compare function: if test-x and test-y are evaluatable, 
;   TCP #t evaluated the same as test-x, and TCP #f evaluated the same as test-y
;   return #t; else return #f
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

; Calls the test-compare-expr with built in test cases
(test-compare-expr test-x test-y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The compare-expr of test-x and test-y gives:
;   Note that this is the test result when given the last if/2 vs if/3 comparisons
;   Current tests comment this comparison, as if/2 need !r6rs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (equal? (compare-expr test-x test-y)

; '(cons
;   (+
;    ((lambda (a b c) (if a (if TCP b c) (if TCP c b)))
;     TCP
;     (if #t (let ((d (if TCP 13 14))) (+ d 15)) 27)
;     (car (cons (if TCP 21 2) (cons 3 (if TCP '(45 78) '(45 3))))))
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
; (compare-expr ''(if x y z) 
;               ''(g x y z))
;               '(if TCP '(if x y z) '(g x y z)))
; (equal?
; (compare-expr ''(if x y z) 
;               '''(g x y z))
;               '(if TCP '(if x y z) ''(g x y z)))

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

