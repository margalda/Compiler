

(define until-shevet 
(lambda (insts deps)
	;(begin (newline)(newline)(display insts)(newline)(display deps)
	(let ((a (filter (lambda (t) (not (equal? (void) t))) (map (lambda (x y) (if   (ormap (lambda(t) (member t y) ) (remww_remww x)) x (void))) insts deps))))
	a
	)
;)
))
(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                    (cdr e))))))

 (define (tree-eq?  t1 t2)
   (or (eq? t1 t2)
       (and (pair? t1) (pair? t2)
            (tree-eq? (car t1) (car t2))
            (tree-eq? (cdr t1) (cdr t2)))))








(define remww_readfrom cadr)
(define remww_remww caddr)
(define remww_last car)


(define remww
	(lambda (lst-of-insts)
		(let* ((dep  (cdr (fold-right 
							(lambda (x xs) `(,(dedupe (append (remww_readfrom x) (filter (lambda (t) (not (member t (remww_remww x)))) (remww_last xs)))) ,@xs)) 
							`(,(dedupe (fold-left (lambda (xs x) `( ,@(remww_remww x) ,@(remww_readfrom x)  ,@xs) ) '() lst-of-insts)))
							lst-of-insts)))
				(ans (until-shevet lst-of-insts  dep))
				)
		(if (tree-eq? lst-of-insts ans) lst-of-insts (remww ans))
		 
		)
	) 
)
(define (deep-filter f lst)
  (cond
    ((null? lst) '())
    ((and (atom? lst) (f lst)) (list lst))
    ((atom? lst) '())
    (else (append (deep-filter f (car lst)) 
                  (deep-filter f (cdr lst))))))

(define add-last 
	(lambda (element lst) (reverse (cons element (reverse lst))))
)