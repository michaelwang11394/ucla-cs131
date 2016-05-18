(define x 0) ; dummy value - will be used to store continuation later
(define y 0)

(+ 2 (call/cc (lambda (cc)
                (set! x cc)  ; set x to the continuation cc; namely, (+ 2 _)
                3)))         ; returns 5

(x 4) ; returns 6

(* 123 (+ 345 (* 789 (x 5)))) ; returns 7


(set! y (+ (* 2 (x 5)) 5)) ; returns 7

(+ y 2)

; cons vs list

(define mylist '(1 2 3))
(cons 0 mylist)
(list 0 mylist)

(cons 1 2)
(list 1 2)
(cons 1 (cons 2 '()))

(define retry #f) 

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))

(factorial 6)
(retry 1)
(retry 2)

(+ 1 2 3)

(define count-pairs-mcguffin
  (lambda (OBJ PROC)
    (call/cc
      (lambda (break)
	(letrec 
	  ((cpm-helper (lambda (o p n)
			 (if (empty? o) n
			   (if (pair? (car o))
			     (+ n
				   (cpm-helper (car o) p 0)
				   (cpm-helper (cdr o) p 0))
			     (if (p (car o))
			       (break (car o))
			       (cpm-helper (cdr o) p (+ n 1))))))))
	  (cpm-helper OBJ PROC 0))))))

(count-pairs-mcguffin '(a b c) symbol?)
(count-pairs-mcguffin '(a b 2) number?)
(count-pairs-mcguffin '(a b c) number?)
(count-pairs-mcguffin '(a b c) boolean?)
(count-pairs-mcguffin '((a b) d c) pair?)
(count-pairs-mcguffin '(#f a b c) (lambda (x) x))
(count-pairs-mcguffin '(0.1 b c) symbol?)
(count-pairs-mcguffin '((0.1 a) b c) symbol?)