 
;;ass1
(load "pc.scm")

(define <boolean>   
	(new

		(*parser (char #\#)) 
		(*parser (char-ci #\f))
		(*caten 2)
		(*pack-with (lambda(x y) #f))

		(*parser (char #\#))
		(*parser (char-ci #\t))
		(*caten 2)
		(*pack-with (lambda(x y) #t))

		(*disj 2)
		done))


(define <charprefix>
	(new
		(*parser (char #\#)) 
		(*parser (char #\\))
		(*caten 2)
		done))


(define <hexchar>
	(new 
		(*parser(range #\0 #\9))
		(*pack (lambda(x)(- (char->integer x) 48)))
		(*parser(range #\a #\f))
		(*pack (lambda(x)(- (char->integer x) 87)))
		(*parser(range #\A #\F))
		(*pack (lambda(x)(- (char->integer x) 55)))
		(*disj 3)
		done))


(define <namedchar>
	(new 
		(*parser(word-ci "lambda"))
		(*pack (lambda (_) (integer->char 955)))
		(*parser(word-ci "newline"))
		(*pack (lambda (_) #\newline))
		(*parser(word-ci "nul"))
		(*pack (lambda (_) #\nul))
		(*parser(word-ci "page"))
		(*pack (lambda (_) #\page))
		(*parser(word-ci "return"))
		(*pack (lambda (_) #\return))
		(*parser(word-ci "space"))
		(*pack (lambda (_) #\space))
		(*parser(word-ci "tab"))
		(*pack (lambda (_) (integer->char 9)))
		(*disj 7)
		done))


(define <visiblesimplechar>
	(range #\! #\~))


(define <hexunicodechar>
	(new
		(*parser (char #\x))
		(*parser <hexchar>) *plus
		(*guard (lambda(x)(> 1114112 (listval x))))
		(*caten 2)
		(*pack-with (lambda(x xs)(integer->char (listval xs))))
		done))


(define <char>
	(new
		(*parser <charprefix>)
		(*parser <namedchar>)
		(*parser <hexunicodechar>)
		(*parser <visiblesimplechar>)
		(*disj 3)
		(*caten 2)
		(*pack-with (lambda(x xs) xs))
		done))


(define <natural>
	(new
		(*parser (range #\0 #\9)) *plus
		(*pack (lambda (x)  (string->number (list->string x))))
		done))


(define <integer>
	(new
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser <epsilon>)
		(*disj 3)
		(*parser <natural>)
		(*caten 2)
		(*pack-with (lambda(x xs) (if (equal? x #\-) (- xs) xs)))
		done))


(define <fraction>
	(new 
		(*parser <integer>)
		(*parser (char #\/))
		(*parser <natural>)
		(*caten 3)
		(*pack-with (lambda(x y z)(/ x z)))
		done))


(define <number>
	(new 
		(*parser <fraction>)
		(*parser <integer>)
		(*disj 2)
		(*parser <any-char>)
		(*parser (char #\)))
	(*delayed (lambda () <skip>))
	(*parser (char #\())
		(*parser (char #\,))
		(*parser (char #\)))
	(*parser (char #\+))
	(*parser (char #\-))
	(*parser (char #\/))
	(*parser (char #\[))    
	(*parser (char #\]))
	(*parser (char #\^))    
	(*parser (char #\*))
	(*parser (char #\#))
	(*parser (char #\]))
	(*disj 14)
	*diff
	*not-followed-by
	done))


(define list-val 
	(lambda(x iter string-len)
		(if (null? x) 0
			(+ (* (car x) (expt 16 (- string-len iter))) (list-val (cdr x) (+ iter 1) string-len))
			)))


(define listval 
	(lambda(x) (list-val x 0 (- (length x) 1))))


(define <stringhexchar>
	(new
		(*parser (word-ci "\\x"))
		(*parser <hexchar>) *star
		(*guard (lambda(x)(> 1114112 (listval x))))
		(*pack (lambda(x)(integer->char (listval x))))
		(*parser (char #\;))
		(*caten 3)
		(*pack-with (lambda(x xs xss) (make-string 1 xs)))
		done))


(define <stringliteralchar>
	(new
		(*parser <any-char>)
		(*parser (char #\\))
		*diff
		(*pack (lambda(x) (make-string 1 x)))
		done))


(define <stringmetachar>
	(new
		(*parser (word "\\\\"))
		(*pack (lambda (_) "\\"))
		(*parser (word "\\\""))
		(*pack (lambda (_) "\""))
		(*parser (word-ci "\\t"))
		(*pack (lambda (_) "\t"))
		(*parser (word-ci "\\f"))
		(*pack (lambda (_) "\f"))
		(*parser (word-ci "\\n"))
		(*pack (lambda (_) "\n"))
		(*parser (word-ci "\\r"))
		(*pack (lambda (_) "\r"))
		(*disj 6)
		done))


(define <stringchar>
	(new
		(*parser <stringhexchar>)
		(*parser <stringmetachar>)
		(*parser <stringliteralchar>)
		(*disj 3)
		done))


(define <string>
	(new
		(*parser (char #\"))
		(*parser <stringchar>)
		(*parser (char #\"))
		*diff
		*star
		(*parser (char #\"))
		(*caten 3)
		(*pack-with (lambda (x y z) (fold-right string-append "" y)))
		done))


(define <symbolchar>
	(new
		(*parser (range #\0 #\9))
		(*parser (range-ci #\a #\z))
		(*pack (lambda(x)(char-downcase x)))
		(*parser (char #\! ))
		(*parser (char #\$ ))
		(*parser (char #\^ ))
		(*parser (char #\* ))
		(*parser (char #\- ))
		(*parser (char #\_ ))
		(*parser (char #\= ))
		(*parser (char #\+ ))
		(*parser (char #\< ))
		(*parser (char #\> ))
		(*parser (char #\? ))
		(*parser (char #\/ ))
		(*parser (char #\: )) 
		(*disj 15)
		done))


(define <symbol>
	(new
		(*parser <symbolchar>)
		*plus
		(*pack (lambda(x) (string->symbol (list->string x))))
		done))


(define <properlist>
	(new
		(*parser (char #\( ))
			(*delayed (lambda () <sexpr>))
			(*delayed (lambda () <skip>)) (*pack (lambda (_)'skip))
			(*disj 2)
			*star
			(*pack (lambda(x)(fold-left (lambda (ys y) (if (equal? y 'skip) ys (append ys (list y)) )) '() x)))
			(*parser (char #\) ))
		(*caten 3)
		(*pack-with (lambda (x y z) `(,@y)))
		done))


(define <improperlist>
	(new
		(*parser (char #\())
			(*delayed (lambda () <sexpr>))
			*plus
			(*parser (char #\.))
			(*delayed (lambda () <sexpr>))
			(*parser (char #\) ))
		(*caten 5)
		(*pack-with (lambda (x1 x2 x3 x4 x5)   `(,@x2 . ,x4) ))
		done))


(define <vector>
	(new
		(*parser (word "#(" ))
		(*delayed (lambda () <sexpr>))
		*star
		(*parser (char #\) ))
	(*caten 3)
	(*pack-with (lambda(x y z) (list->vector y)))
	done))


(define <quoted>
	(new
		(*parser (char #\' ))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (x y) (list 'quote y)))
		done))


(define <quasiquoted>
	(new
		(*parser (char #\`))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (x y) (list 'quasiquote y)))
		done))


(define <unquoted>
	(new
		(*parser (char #\, ))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (x y ) (list 'unquote y)))
		done))


(define <unquotedandspliced>
	(new
		(*parser (char #\, ))
		(*parser (char #\@ ))
		(*delayed (lambda () <sexpr>))
		(*caten 3)
		(*pack-with (lambda (x y z) (list 'unquote-splicing z)))
		done))


(define <whitespace>
	(const
		(lambda (ch)
			(char<=? ch #\space))))


(define <white+>
	(new
		(*parser <whitespace>) *plus
		done))


(define <white*>
	(new
		(*parser <whitespace>) *star
		done))


(define <line-comment>
	(let ((<end-of-line-comment>
		(new (*parser (char #\newline))
			(*parser <end-of-input>)
			(*disj 2)
			done)))
	(new (*parser (char #\;))

		(*parser <any-char>)
		(*parser <end-of-line-comment>)
		*diff *star

		(*parser <end-of-line-comment>)
		(*caten 3)
		done)))


(define <sexpr-comment>
	(new (*parser (word "#;"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		done))


(define <comment>
	(disj <line-comment>
		<sexpr-comment>))


(define <skip>
	(new (*parser <comment>)
		(*parser <whitespace>)
		(*disj 2) 
		done))   


(define <sexpr-infix-comment>
	(new (*parser (word "#;"))
		(*delayed (lambda () <infixexpression>))
		(*caten 2)
		done))


(define <comment-infix>
	(disj <line-comment>
		<sexpr-infix-comment>))


(define <skip-infix>
	(new (*parser <comment-infix>)
		(*parser <whitespace>)
		(*disj 2) 
		done))   


(define <sexpr>
	(new
		(*parser <skip>)*star
		(*parser <boolean>)
		(*parser <char>)
		(*parser <number>)
		(*parser <visiblesimplechar>)
		(*parser (char #\)))
	*diff
	*not-followed-by
	(*parser <string>)
	(*parser <symbol>)
	(*delayed (lambda() <infixextension>))
	(*delayed (lambda() <properlist>))
	(*delayed (lambda() <improperlist>))
	(*delayed (lambda() <vector>))
	(*delayed (lambda() <quoted>))
	(*delayed (lambda() <quasiquoted>))
	(*delayed (lambda() <unquotedandspliced>))
	(*delayed (lambda() <unquoted>))
	(*disj 13)
	(*parser <skip>)*star
	(*caten 2) (*pack-with (lambda ( y z) y))
	(*caten 2) (*pack-with (lambda ( z x ) x))
	done))


(define (char->symbol ch)
	(string->symbol (string ch)))


(define rec-symbol
	(lambda (first rest)
		(if (= (length rest) 1) (list (if (or (equal? (caar rest) '^) (equal? (caar rest) '**)) 'expt (char->symbol(caar rest))) first (cadar rest)) 
			(fold-left
				(lambda (xs x) (list (if (or (equal? (car x) '^) (equal? (car x) '**))'expt (char->symbol(car x))) xs (cadr x)) )
				(list (if (or (equal? (caar rest) '^) (equal? (caar rest) '**)) 'expt (char->symbol(caar rest))) first (cadar rest))
				(cdr rest)))))


(define rec-pow
	(lambda (first rest)
		`(,(if (or (equal? (caar rest) '^) (equal? (caar rest) '**)) 'expt (char->symbol(caar rest))) ,first ,(if (= 1 (length rest)) (cadar rest) (rec-pow (cadar rest) (cdr rest)) )
			)))


(define <infixneg>
	(new
		(*parser (char #\-))
		(*parser <skip>) *star
		(*delayed (lambda()<infixarrayget>))
		(*delayed (lambda()<escapeinfix>))
		(*delayed (lambda()<infixfuncall>))
		(*delayed (lambda()<infixparen>))
		(*delayed (lambda()<number>))
		(*delayed (lambda()<infixsymbol>))

		(*disj 6)
		(*caten 3)
		(*pack-with (lambda (x y z) (if (and (null? y) (number? z)) (- z) `(-,z))))
		done))


(define <powersymbol>
	(new
		(*parser (char #\^))
		(*pack (lambda(_) '^))
		(*parser (word "**"))
		(*pack (lambda(_) '**))
		(*disj 2)
		done))


(define <escapeinfix>
	(new
		(*delayed (lambda () <infixprefixextentionprefix>))
		(*pack(lambda(x)'problem))
		(*parser <sexpr>)
		(*caten 2)
		(*pack-with(lambda(x y)y))
		done))


(define <thirdorder>
	(new
		(*parser <skip-infix>) *star
		(*delayed (lambda() <infixarrayget>))	
		(*parser <escapeinfix>)
		(*parser <infixneg>)
		(*delayed (lambda() <infixfuncall>))
		(*parser <number>)
		(*delayed (lambda () <infixparen>))
		(*delayed (lambda () <infixsymbol>))
		(*disj 7)
		(*parser <skip-infix>) *star
		(*caten 3) (*pack-with (lambda (z x y) x))
		done))


(define <secondorder>
	(new
		(*parser <thirdorder>)
		(*parser <powersymbol>) (*parser <thirdorder>) (*caten 2)
		*star 
		(*caten 2)
		(*pack-with (lambda(x y) (if (null? y) x (rec-pow x y))))
		done))


(define <firstorder>
	(new

		(*parser <secondorder>)
		(*parser (char #\*))
		(*parser (char #\/))
		(*disj 2)
		(*parser <secondorder>)
		(*caten 2) 
		*star
		(*caten 2)
		(*pack-with (lambda(x y) (if (null? y) x (rec-symbol x y))))
		done))


(define <infixexpression>
	(new
		(*parser <firstorder>)
		(*parser (char #\+))
		(*parser (char #\-))
		(*disj 2)
		(*parser <firstorder>)
		(*caten 2) *star
		(*caten 2)
		(*pack-with (lambda(x y) (if (null? y) x (rec-symbol x y))))
		done))


(define <infixparen>
	(new
		(*parser (char #\())
			(*delayed (lambda()<infixexpression>))
			(*parser (char #\)))
		(*caten 3)
		(*pack-with (lambda(x y z)y))
		done))


(define <infixsymbol>
	(new
		(*parser (range #\0 #\9))
		(*parser (range-ci #\a #\z))
		(*pack (lambda(x)(char-downcase x)))
		(*parser (char #\! ))
		(*parser (char #\$ ))
		(*parser (char #\_ ))
		(*parser (char #\= ))
		(*parser (char #\< ))
		(*parser (char #\> ))
		(*parser (char #\? ))
		(*disj 9)
		*plus
		(*parser(word "##"))
		(*parser(word "#%"))
		(*disj 2)
		*diff
		(*pack (lambda (x) (string->symbol (list->string x))))
		done))


(define <infixarglist>
	(new
		(*delayed (lambda()<infixexpression>))
		(*parser (char #\,))
		(*delayed (lambda()<infixexpression>))
		(*caten 2)
		(*pack-with (lambda (x y) y)) *star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))
		(*parser <epsilon>)
		(*disj 2)
		done))


(define <arrayargs>
	(new
		(*parser (char #\[))
		(*parser <infixexpression>)
		(*parser (char #\]))
		(*caten 3)
		(*pack-with (lambda(x y z) `('array ,y)))
		done))


(define array-rep
	(lambda (x2 y) (if (= (length y) 1) (list 'vector-ref x2 (cadar y)) 
		(fold-left
			(lambda(xs x) (if (equal? (cadar x) 'array)`(vector-ref ,xs ,(cadr x)) `(,xs  ,@(cadr x))))
			(list 'vector-ref x2 (cadar y))
			(cdr y)))))   
(define func-rep
	(lambda (x2 y) (if (= (length y) 1) `(,x2 ,@(cadar y)) 
		(fold-left
			(lambda(xs x) (if (equal? (cadar x) 'array)`(vector-ref ,xs ,(cadr x)) `(,xs  ,@(cadr x))))
			`(,x2 ,@(cadar y))
			(cdr y)))))


(define <funcallargs2>
	(new
		(*parser (char #\())
			(*parser <skip-infix>) *star
			(*parser <infixarglist>)
			(*parser <skip-infix>) *star
			(*parser (char #\)))
		(*caten 5) 
		(*pack-with(lambda(x1 x2 x3 x4 x5) `('func ,x3)))
		done))


(define <infixfuncall>
	(new
		(*parser (char #\-))
		(*parser <number>)
		(*caten 2)
		(*pack-with (lambda(x y) (- y)))

		(*parser <skip-infix>) *star

		(*parser <funcallargs2>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))


		(*caten 3)
		(*pack-with (lambda (x1  x2  x3  ) (func-rep x1 x3)))

		(*parser (char #\-))
		(*parser <infixparen>)
		(*delayed  (lambda() <infixarrayget>))
		(*delayed (lambda()<infixsymbol>))
		(*parser <escapeinfix>)
		(*disj 4)
		(*caten 2)

		(*parser <skip-infix>) *star

		(*parser <funcallargs2>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))


		(*caten 3)
		(*pack-with (lambda (x1  x2  x3  ) `(- ,(func-rep x1 x3))))

		(*parser <number>)
		(*parser <infixparen>)
		(*delayed (lambda()<infixsymbol>))
		(*parser <escapeinfix>)
		(*disj 4)

		(*parser <skip-infix>) *star

		(*parser <funcallargs2>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))

		(*caten 3)
		(*pack-with (lambda (x1  x2  x3  ) (func-rep x1 x3)))

		(*disj 3)

		done))


(define <infixarrayget>
	(new
		(*parser (char #\-))
		(*parser <number>)
		(*caten 2)
		(*pack-with (lambda(x y) (- y)))
		(*parser <skip>) *star

		(*parser <arrayargs>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))		
		(*caten 3)
		(*pack-with (lambda (x1 x2 x3) (array-rep x1 x3)))

		(*parser (char #\-))
		(*parser <infixparen>)
		(*parser <infixfuncall>)
		(*delayed (lambda()<infixsymbol>))
		(*parser <escapeinfix>)
		(*disj 4)
		(*caten 2)
		(*parser <skip>) *star

		(*parser <arrayargs>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))

		(*caten 3)
		(*pack-with (lambda (x1 x2 x3) `(- ,(array-rep (cadr x1) x3))))

		(*parser <number>)
		(*parser <infixparen>)
		(*delayed (lambda()<infixsymbol>))
		(*parser <escapeinfix>)
		(*disj 4)
		(*parser <skip>) *star

		(*parser <arrayargs>)    
		(*parser <arrayargs>)
		(*parser <funcallargs2>)
		(*disj 2)
		*star
		(*caten 2)
		(*pack-with (lambda(x y) `(,x ,@y)))

		(*caten 3)
		(*pack-with (lambda (x1 x2 x3) (array-rep x1 x3)))

		(*disj 3)
		done))


(define <infixprefixextentionprefix>
	(new
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
		done))


(define <infixextension>
	(new
		(*parser <infixprefixextentionprefix>)
		(*parser <infixexpression>)
		(*caten 2)
		(*pack-with (lambda (x y)y))    
		done))


;;ass2
(load "pattern-matcher.scm")
(load "qq.scm")

(define *reserved-words* 
	'(and begin cond define do else if lambda 
		let let* letrec or quasiquote unquote 
		unquote-splicing quote set!))

(define duplicate?
	(lambda (lst)
		(cond   ((null? lst) #f)
			((member (car lst) (cdr lst)) #t)
			(else (duplicate? (cdr lst)))
			)))

(define duplicate-arg?
	(lambda (lst)
		(cond   ((null? lst) #f)
			((member (car (car lst)) (map car (cdr lst))) #t)
			(else (duplicate? (cdr lst)))
			)))


(define improper->list 
	(lambda (lst) (if (pair? lst) 
		(cons (car lst) (improper->list (cdr lst))) 
		(list lst)) 
	))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))
			(else (identify-lambda (cdr argl)
				(lambda (s) (ret-simple `(,(car argl) ,@s)))
				(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
				(lambda (var) (ret-opt `(,(car argl)) var)))))))


(define modify-args
	(lambda (x) (identify-lambda x
		(lambda (s) `(,s))
		(lambda (s opt) `(,s ,opt))
		(lambda (s) `(,s))))
	)


(define improper-check
	(lambda (x) (if (not (pair? x)) 
		(var? x) 
		(and (var? (car x)) (improper-check (cdr x)))   
		))
	)


(define var? 
	(lambda (x) 
		(and (symbol? x) 
			(not (member x *reserved-words*)))))



(define remove-nested (lambda (lst)    
	(if (pair? lst) 
		(append '()  
			(cond ((equal? (car lst) 'begin) '())
				((and (pair? (car lst)) (equal? (caar lst) 'begin))(remove-nested (car lst)))
				(else (list (parse (car lst))))) 
			(remove-nested (cdr lst))) 
		(if (or (equal? lst 'begin)(null? lst )) '() (list  lst)) )

	))

(define get-args 
	(lambda (ribs)
		(map car ribs))) 


(define parse
	(let ((run 
		(compose-patterns
				;;const
				(pattern-rule
					(? 'c const?)
					(lambda (c)
						(if (quote? c)
							`(const ,(cadr c))
							`(const ,c))))


				;;if
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif)) 
					(lambda (test dit dif)
						`(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit))
					(lambda (test dit)
						`(if3 ,(parse test) ,(parse dit) (const ,(void)))))
				

				;;var
				(pattern-rule
					(? 'v var?)
					(lambda (v)
						`(var ,v)))


                ;;or
                (pattern-rule
                	`(or . ,(? 'exprs)) 
                	(lambda (exprs)
                		(cond 
                			((= (length exprs) 0) `(const #f))
                			((= (length exprs) 1) (parse (car exprs)))
                			(else `(or ,(map parse exprs)))
                			)))

                
                 ;;regular-lambda
                 (pattern-rule
                 	`(lambda ,(? 'vars (lambda (x) (and (list? x) (not (duplicate? x)) (andmap var? x)))) . ,(? 'body))
                 	(lambda (vars body)
                 		`(lambda-simple ,@(modify-args vars) ,(parse `(begin ,@body)))))


                ;;optional-lambda
                (pattern-rule
                	`(lambda ,(? 'vars  (lambda (lst) (and (pair? lst) (not (list? lst)) (improper-check lst) (not (duplicate? (improper->list lst))))))  . ,(? 'body))
                	(lambda (vars body)
                		`(lambda-opt ,@(modify-args vars) ,(parse `(begin ,@body)))))


                 ;;variadic-lambda
                 (pattern-rule
                 	`(lambda ,(? 'vars var?) . ,(? 'body))
                 	(lambda (vars body)
                 		`(lambda-var ,@(modify-args vars) ,(parse `(begin ,@body)))))


                ;;begin
                (pattern-rule
                	`(begin . ,(? 'exprs)) 
                	(lambda (exprs)
                		(cond 
                			((= (length exprs) 0) `(const ,(void)))
                			((= (length exprs) 1) (parse (car exprs)))
                			(else `(seq ,(remove-nested exprs)))
                			)))


                ;;define
                (pattern-rule
                	`(define ,(? 'var var?) . ,(? 'expr))
                	(lambda (var expr)
                		`(def ,(parse var) ,(parse `(begin ,@expr))) ))


                ;;MIT-define
                (pattern-rule
                	`(define (,(? 'var var?) . ,(? 'other-vars)) . ,(? 'expr))
                	(lambda (var other-vars expr)
                		`(def ,(parse var) ,(parse `(lambda ,other-vars ,@expr)))))


                ;;set!
                (pattern-rule
                	`(set! ,(? 'var var?) ,(? 'expr))
                	(lambda (var expr)
                		`(set ,(parse var) ,(parse expr))))


                ;;and->if
                (pattern-rule
                	`(and . ,(? 'exprs)) 
                	(lambda (exprs)
                		(cond 
                			((= (length exprs) 0) `(const #t))
                			((= (length exprs) 1) (parse (car exprs)))
                			(else (parse `(if ,(car exprs) (and ,@(cdr exprs)) #f))
                				))))


                ;;cond->if
                (pattern-rule
                	`(cond ,(? 'first) . ,(? 'rest)) 
                	(lambda (first rest)
                		(if (null? rest)
                			(if (equal? 'else (car first))
                				(parse `(begin ,@(cdr first)))
                				(parse `(if ,(car first) (begin ,@(cdr first)))))
                			(parse `(if ,(car first) (begin ,@(cdr first)) (cond ,@rest)))
                			)))
                

                ;;let->lambda
                (pattern-rule
                	`(let ,(? 'ribs (lambda (x) (not (duplicate-arg? x)))) . ,(? 'body))
                	(lambda (ribs body)
                		(parse `((lambda ,(map car ribs)
                			(begin ,@body)) ,@(map cadr ribs)))))
                

                ;;let*->let			
                (pattern-rule
                	`(let* ,(? 'ribs) . ,(? 'body))
                	(lambda (ribs body)
                		(if (< (length ribs) 2)
                			(parse `(let ,ribs ,@body))
                			(parse `(let (,(car ribs))
                				(let* ,(cdr ribs) ,@body))))))


                ;;letrec->let
                (pattern-rule
                	`(letrec ,(? 'ribs) . ,(? 'body))
                	(lambda (ribs body)
                		(parse `(let ,(map (lambda (args)
                			(list (car args) #f)) ribs)
                		(begin ,@(map (lambda (args)
                			`(set! ,(car args) ,(cadr args))) ribs)
                		(let () ,@body))))))


                ;;quasiquote
                (pattern-rule
                	`(quasiquote . ,(? 'exprs))
                	(lambda (exprs)
                		(parse (expand-qq (car exprs)))
                		))

                ;;application
                (pattern-rule
                	`(,(? 'func (lambda (x) (not (member x *reserved-words*)))) .,(? 'args))
                	(lambda (func args)
                		`(applic ,(parse func) ,(map parse args))))


                )))
(lambda (sexpr)
	(run sexpr (lambda () "ERROR")))))

;;ass3
(define split
	(lambda (pes ret-ds-es)
		(if (null? pes) (ret-ds-es '() '())
			(split
				(cdr pes)
				(lambda (ds es)
					(cond ((eq? (caar pes) 'def)
						(ret-ds-es (cons (car pes) ds) es))
					((eq? (caar pes) 'seq)
						(split (cadar pes)
							(lambda (ds1 es1)
								(ret-ds-es (append ds1 ds)
									(append es1 es)))))
					(else (ret-ds-es ds (cons (car pes) es)))))))))


(define lambda?
	(lambda (exp) (and (not (null? exp)) (list? exp) (or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt) (equal? (car exp) 'lambda-var) )) )
	)


(define get-ds+es
	(lambda (pexpr)
		(split (get-body pexpr) (lambda (x y)(list x y)))
		))


(define eliminate-nested-defines 
	(lambda (pexpr)
		(cond ((or (null? pexpr)(not (list? pexpr))) pexpr)
			((lambda? pexpr) 
				(let*  ((ds+es (get-ds+es pexpr))
					(ds (car ds+es))
					(es (cadr ds+es)))
				(if (> (length ds) 0)
					(let* 
						((args (ass3_get-args pexpr))
							(list-of-defs-names (map (lambda(x)(cadadr x)) ds))
							(body (map (lambda (x) (eliminate-nested-defines x)) es))
							(sets (map  (lambda (x) `(set ,(cadr x) ,(eliminate-nested-defines (caddr x)))) ds))
							(falses (map (lambda (x) '(const #f)) ds))
							(get-tag (car pexpr)))
						`(,get-tag ,@args (applic (lambda-simple ,list-of-defs-names (seq ,(append sets body))) ,falses)) )

					(map eliminate-nested-defines pexpr) )

				)
				)
			(else (map eliminate-nested-defines pexpr))
			)   
		)
	)


(define applic?
	(lambda (exp) (and (not (null? exp)) (list? exp) (equal? (car exp) 'applic)))
	)


(define remove-applic-lambda-nil
	(lambda (expr)
		(cond   ((or (null? expr)(not (list? expr))) expr)
			((applic? expr) 
				(if (and (lambda? (cadr expr)) (null? (cadr (cadr expr))) (null? (caddr expr)) ) (car(remove-applic-lambda-nil (cddadr expr)))
					(map  remove-applic-lambda-nil  expr)))
			(else (map  remove-applic-lambda-nil  expr)))
		)   

	)


(define ass3_var?
	(lambda (expr) (and (not (null? expr)) (list? expr) (equal? (car expr) 'var)))
	)


(define pe->lex-pe
	(lambda (expr)
		(analyse expr '() '())
		)
	)


(define ass3_get-args
	(lambda (exp) 
		(let* ((tag (car exp)))
			(cond ((equal? tag 'lambda-simple)  (list (cadr exp)))
				((equal? tag 'lambda-var) (list (cadr exp)))
				((equal? tag 'lambda-opt) (list (cadr exp) (caddr exp)))))
		)
	)


(define get-params
	(lambda (exp)
		(let* ((tag (car exp)))
			(cond ((equal? tag 'lambda-simple) (cadr exp))
				((equal? tag 'lambda-var)  (list (cadr exp)))
				((equal? tag 'lambda-opt)  (append (cadr exp) (list (caddr exp))))))
		)
	)


(define get-index
	(lambda (name params)
		(- (length params) (length (member name params)))
		)
	)


(define get-index-nested
	(lambda (name simulated_scope)
		(let* ((binary-list  (map (lambda (x)(if (member name x) #t #f)) simulated_scope))
			(index (- (length binary-list) (length (member #t binary-list)))))

		index
		)
		)
	)


(define pvar? 
	(lambda (var-exp params)
		(let ((name (cadr var-exp)))
			(if (member name params) 
				(let ((minor (get-index name params)))
					`(pvar ,name ,minor)
					)
				#f
				)
			)
		)
	)


(define bvar? 
	(lambda (var-exp simulated_scope)
		(let ((name (cadr var-exp)))
			(if (ormap (lambda (x) (member name x)) simulated_scope) 
				(let* ((major (get-index-nested name simulated_scope ))
					(minor (get-index name (list-ref simulated_scope major))))
				`(bvar ,name ,major ,minor)
				)
				#f
				)
			)
		)
	)


(define bind-var 
	(lambda (var-exp simulated_scope params)
		(cond   ((pvar? var-exp params) (pvar? var-exp params))
			((bvar? var-exp simulated_scope) (bvar? var-exp simulated_scope))
			(else `(fvar ,(cadr var-exp))))
		)
	)


(define analyse
	(lambda (ast simulated_scope params)
		(cond ((lambda? ast) (set! simulated_scope `(,params ,@simulated_scope))
			(set! params (get-params ast)))
		((ass3_var? ast) (set! ast (bind-var ast simulated_scope params)))
		)

		(if (and (not (null? ast))(list? ast)) (map (lambda (x) (analyse x simulated_scope params)) ast) ast)
		)
	)


(define has-bounded-occurance-nested?
	(lambda (exp arg) 
		(cond ((or (null? exp) (not (list? exp))) (equal? arg exp))
			((ass3-const? exp) #f)
			((and (lambda? exp) (member arg (get-params exp)))  #f) 
			(else (ormap (lambda (x) (has-bounded-occurance-nested? x arg)) exp) ))
		)
	)


(define has-bounded-occurance?
	(lambda (body arg)
		(cond ((or (null? body) (not (list? body))) #f)
			((ass3-const? exp) #f)
			((lambda? body) (has-bounded-occurance-nested? body arg)) 
			(else (ormap (lambda (x) (has-bounded-occurance? x arg)) body) ))
		)
	)


(define set-var?
	(lambda (exp var)
		(and (not (null? exp)) (list? exp) (equal? 'set (car exp)) (= (length exp) 3) (equal? (cadadr exp) var))
		)
	)

(define get-var?
	(lambda (exp var)
		(and (not (null? exp)) (list? exp) (equal? 'var (car exp)) (= (length exp) 2) (equal? (cadr exp) var))
		)
	)


(define has-set-occurance?
	(lambda (body arg)
		(cond ((or (null? body) (not (list? body))) #f)
			((ass3-const? exp) #f)
			((set-var? body arg) #t)
			((and (lambda? body) (member arg (get-params body))) #f)   
			((lambda? body) (has-set-occurance? (get-body body) arg))
			(else (ormap (lambda (x) (has-set-occurance? x arg)) body) ))
		)
	)


(define has-get-occurance?
	(lambda (body arg)
		(cond ((equal? body arg) #t)
			((or (null? body) (not (list? body)) ) #f)
			((ass3-const? exp) #f)
			((set-var? body arg) (has-get-occurance? (caddr body) arg))
			((and (lambda? body) (member arg (get-params body))) #f)   
			((lambda? body) (has-get-occurance? (get-body body) arg))
			(else (ormap (lambda (x) (has-get-occurance? x arg)) body) ))
		)
	)


(define get-body
	(lambda (exp) 
		(let* ((tag (car exp)))
			(cond   ((equal? tag 'lambda-simple)  (cddr exp))
				((equal? tag 'lambda-var) (cddr exp))
				((equal? tag 'lambda-opt) (cdddr exp))))
		)
	)


(define box-get? 
	(lambda (exp)
		(if (and (not(null? exp)) (list? exp) (equal? (car exp) 'box-get) ) #t #f)
		)
	)


(define box-set? 
	(lambda (exp)
		(if (and (not(null? exp)) (list? exp) (equal? (car exp) 'box-set) ) #t #f)
		)
	)


(define replace-body 
	(lambda(body var)
		(cond ((or (null? body) (not (list? body)) (box-get? body)) body)
			((box-set? body) `(box-set ,(cadr body) ,(replace-body (caddr body) var)))
			((get-var? body var) `(box-get ,body))
			((set-var? body var) `(box-set ,(cadr body) ,(replace-body (caddr body) var) ))
			((and (lambda? body) (member var (get-params body))) body) 
			((lambda? body) `(,(car body) ,@(ass3_get-args body)  ,@(replace-body (get-body body) var)))
			(else (map (lambda (x) (replace-body  x var)) body) ))
		)
	)                           


(define box-var 
	(lambda (body var)
		(let* ( (a (has-get-occurance? body var))
			(b (has-set-occurance? body var))
			(c (has-bounded-occurance? body var))
			)
		(if (not (and a b c)) body (if (and (not (null? body))(list? body)(equal? (car body) 'seq))   `(seq ,@(list  `((set (var ,var) (box (var ,var))) ,@(cadr(replace-body body var)))))     `(seq ((set (var ,var) (box (var ,var))) ,(replace-body body var)))))
		)
		)
	)


(define box-set
	(lambda (exp)
		(cond ((or (null? exp) (not (list? exp))) exp)
			((and (lambda? exp) (not (null? (get-params exp)))) `(,(car exp) ,@(ass3_get-args exp) 
				,(fold-right (lambda (var body) (box-var body var))
					(box-set (car (get-body exp))) 
					(get-params exp))))

			(else (map box-set exp))
			)
		)
	)


(define ass3-const? 
	(lambda (exp)
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'const)) #t #f)
		)
	)


(define if?
	(lambda (exp) 
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'if3)) #t #f)
		)
	)

(define seq?
	(lambda (exp) 
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'seq)) #t #f)
		)
	)


(define or?
	(lambda (exp) 
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'or)) #t #f)
		)
	)


(define define?
	(lambda (exp) 
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'def)) #t #f)
		)
	)


(define set?
	(lambda (exp) 
		(if (and (not (null? exp)) (list? exp) (equal? (car exp) 'set)) #t #f)
		)
	)


(define annotate
	(lambda (exp tp?)
		(cond ((or (ass3_var? exp) (ass3-const? exp)) 
			exp)
		((applic? exp) 
			(if tp? `(tc-applic ,(annotate (cadr exp) #f) ,(map (lambda (x) (annotate x #f)) (caddr exp))) 
				`(applic ,(annotate (cadr exp) #f) ,(map (lambda (x) (annotate x #f)) (caddr exp)))
				))
		((or (or? exp) (seq? exp)) 
			`(,(car exp) ,(append (map (lambda (x) (annotate x #f)) (reverse (cdr (reverse (cadr exp))))) (list (annotate (car (reverse (cadr exp))) tp?))))
			)
		((if? exp) 
			`(if3 ,(annotate (cadr exp) #f) ,(annotate (caddr exp) tp?) ,(annotate (cadddr exp) tp?))
			)
		((define? exp) 
			`(def ,(cadr exp) ,(annotate (caddr exp) #f))
			)
		((lambda? exp) 
			`(,(car exp) ,@(ass3_get-args exp) ,(annotate (car (get-body exp)) #t))
			)
		((set? exp) `(set ,(cadr exp) ,(annotate (caddr exp) #f))
			)
		((or (set? exp) (box-set? exp)) 

			`(,(car exp) ,(cadr exp) ,(annotate (caddr exp) #f))
			)
		(else exp))
		)
	)


(define annotate-tc
	(lambda (exp)
		(annotate exp #f)))