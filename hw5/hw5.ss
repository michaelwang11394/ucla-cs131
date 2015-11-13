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

(define (compare-expr-decision x y)
  (if (and (list? x) (list? y))
    (if (equal? (length x) (length y))
      (if (equal? (car x) (car y))
        (match (car x)
          ['quote (compare-expr-quote x y)]
          ;['if (compare-expr-if x y)]
          ['lambda (compare-expr-lambda x y)]
          ['let (compare-expr-let x y)]
          [_ (compare-expr-list x y)])
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
        (cons (compare-expr-decision (car x) (car y)) (compare-expr-list (cdr x) (cdr y)))))
))

(compare-expr-decision 12 20)
(compare-expr-decision 12 12)
(compare-expr-decision #f #t)
(compare-expr-decision #t #f)
(compare-expr-decision #f #f)

;(compare-expr-quote '(quote (a b)) '(quote (a c)))

(compare-expr-decision '(12 20) '(12 12))
(compare-expr-decision '(12 20 30) '(12 12))
(compare-expr-decision '(12 (20 40) 30) '(12 (20 30) 12))

(compare-expr-decision '(quoth (a b)) '(quoth (a c)))
(compare-expr-decision '(12 '(20 40) 30) '(12 '(20 30) 12))
(compare-expr-decision '(quote (if x y z)) '(quote (if x z y)))

(compare-expr-decision '(if x y z) '(if x z z))
(compare-expr-decision '(if x y z) '(if x z y))
(compare-expr-decision '(if y y z) '(if x z z))
(compare-expr-decision '(if x #t) '(if x #f))

(compare-expr-decision '(if x y z) 
                       '(g x y z))
(compare-expr-decision '(quote (if x y z)) 
                       '(g (if x y z)))
(compare-expr-decision '(lambda x x z) 
                       '(g x x z))
(compare-expr-decision '(quote (if x y z)) 
                       '(g (if x y z) 20))

(compare-expr-decision '((lambda (a) (f a)) 1) 
                       '((lambda (a) (g a)) 2))
(compare-expr-decision '((lambda (a b) (f a b)) 1 2) 
                       '((lambda (a c) (f c a)) 1 2))
(compare-expr-decision '((lambda (a b) (f a b)) 1 2) 
                       '((lambda (a c) (f c a)) 2 1))

(compare-expr-decision '(cons a b) 
                       '(cons a c))
(compare-expr-decision '(cons (cons a b) (cons b c)) 
                       '(cons (cons a c) (cons a c)))
(compare-expr-decision '(cons a b) 
                       '(list a b))

(compare-expr-decision '(let ((a 1)) (f a)) 
                       '(let ((a 2)) (g a)))

(compare-expr-decision '(+ #f (let ((a 1) (b 2)) (f a b)))
                       '(+ #t (let ((a 1) (c 2)) (f a c))))


;(cons 2 (cons 3 (cons 4 '())))

;(returnlist4 2 3 4 5)