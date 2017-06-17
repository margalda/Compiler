	
(load "ass3.scm")
;; ------------------------- FINAL PROJECT -------------------------------


;; Check compiler.scm with MAYER compiled program


(define string->sexprs
	(lambda (s)
		(<sexpr> (string->list (string-append "(" (string-append s ")")))
			(lambda (e s)
				e)
			(lambda (w) `(failed with report: ,@w)))))


(define get-prefix-from-parsed
	(lambda (parsed container prefix)
		(cond ((or (not (list? parsed)) (null? parsed)) container)
			((equal? (car parsed) prefix) (append container (list (cadr parsed))))
			(else (fold-left  (lambda (cont exp) (get-prefix-from-parsed exp cont prefix)) container parsed)))
		)
	)

(define subsets 
	(lambda (lst)
		(if (null? lst) 
			'() 
			(if (not (list? lst)) 
				(if (vector? lst) 
					(append (map subsets (vector->list lst)) (list lst))
					lst) 
				(append (fold-left (lambda(xs x) (if (not (list? x)) (append xs (list x)) (append xs (subsets x)))) '() lst)
					(subsets (cdr lst)) (list lst)))
			)))

(define subsets-for-symbols 
	(lambda (lst)
		(if (null? lst) 
			'() 
			(if (not (list? lst)) 
				(if (vector? lst) 
					`( ,@(flatten (map subsets-for-symbols (vector->list lst))) ,lst)
					(if (pair? lst)  (append (subsets-for-symbols (car lst))(subsets-for-symbols (cdr lst)) (list lst)) (list lst)  )) 
				(append (fold-left (lambda(xs x) (if (not (list? x)) (append xs (list x)) (append xs (subsets-for-symbols x)))) '() lst)
					(subsets-for-symbols (cdr lst)) (list lst)))
			)))



(define remove-dups 
	(lambda (lst)
		(fold-left  (lambda (xs x) (if (member x xs) xs (append xs (list x)))) '() lst)
		)
	)

(define member-rec
	(lambda (x lst) 
		(if (equal? x lst) 
			#t 
			(if (list? lst) 
				(ormap (lambda (y) (if (list? y) (member-rec x y) (equal? y x))) lst) 
				(if (vector? lst) 
					(member-rec x (vector->list lst))
					#f)))))

(define number-value
	(lambda (lst value) 
		(if (null? lst) `((2 ,value)) 
			(let* ((prev-num (if (null? lst) 0 (caar lst)))
			(current-num (cond ((equal? (cadar lst) (void)) (+ 1 prev-num))
				((null? (cadar lst))         (+ 1 prev-num))
				((equal? (cadar lst) #t)     (+ 2 prev-num))
				((equal? (cadar lst) #f)     (+ 2 prev-num))
				((integer? (cadar lst))      (+ 2 prev-num))
				((pair? (cadar lst))         (+ 3 prev-num))
				((vector? (cadar lst))       (+ (vector-length (cadar lst)) 2 prev-num))
				((string? (cadar lst))       (+ (string-length (cadar lst)) 2 prev-num))
				((char? (cadar lst))         (+ 2 prev-num))
				((symbol? (cadar lst)) 		 (+ 2 prev-num))
                ((fraction? (cadar lst))     (+ 3 prev-num))
    			))
			(numbered-value `(,current-num ,value)))
		(append (list numbered-value) lst)
		))
		)
	)

(define lookup-table 
	(lambda (table exp)
		(if (null? table)  "ERROR in lookup-table"  (if (equal? (cadar table) exp) (caar table) (lookup-table (cdr table) exp)))
		)
	)

(define evaluate-value 
	(lambda (lst value)
		(let* ((evaluated-value (cond 	((equal? (cadr value) (void)) `(,@value "T_VOID"))
										((null? (cadr value))         `(,@value "T_NIL"))
										((equal? (cadr value) #t)     `(,@value "T_BOOL" 1))
										((equal? (cadr value) #f)     `(,@value "T_BOOL" 0))
										((integer? (cadr value))      `(,@value "T_INTEGER" ,(cadr value)))
										((pair? (cadr value))         `(,@value "T_PAIR" ,(lookup-table lst (car (cadr value))) ,(if (null? (cdr (cadr value))) 3  (lookup-table lst (cdr (cadr value))))))
								        ((vector? (cadr value))       `(,@value "T_VECTOR" ,(vector-length (cadr value)) ,@(map (lambda(x) (lookup-table lst x)) (vector->list (cadr value))))) ; TO-DO: FIX
								        ((string? (cadr value))       `(,@value "T_STRING" ,(string-length (cadr value)) ,@(map char->integer (string->list (cadr value)))))
								        ((char? (cadr value))         `(,@value "T_CHAR" ,(char->integer (cadr value))))
	       							 	((symbol? (cadr value))       `(,@value "T_SYMBOL" 0))
	       							 	((fraction? (cadr value))     `(,@value "T_FRACTION" ,(numerator (cadr value)) ,(denominator (cadr value))))
	                                 )))
		(append lst (list evaluated-value))
		)
		)
	)

(define fraction? rational?)

(define flatten 
	(lambda (lst)
		(if (or (null? lst)(not (list? lst))) lst 
			(fold-left (lambda (xs x)  (if (list? x) (append xs x) (append xs (list x))) ) '() lst)
			)
		)
	)


(define build-constants-table
	(lambda (parsed)
		(let* ((default-constants `(,(void) () #t #f))
			(constants-exps (get-prefix-from-parsed parsed `() 'const))
			(const-subs (flatten (map subsets-for-symbols constants-exps)))
			(symbols (filter symbol? const-subs))
			(symbols-to-strings (map (lambda (x) (if (symbol? x) (symbol->string x) x)) const-subs)) ;; changes all symbols to strings
			(remove-duplicated (remove-dups (append default-constants (append symbols symbols-to-strings))))
			(sorted-constants (sort member-rec remove-duplicated))
			(numbered-const (reverse (fold-left number-value '() sorted-constants)))
			(complete-constant-table (fold-left evaluate-value '() numbered-const)))
			complete-constant-table 
			)
		)
	)

(define build-symbol-table
	(lambda (parsed)
		(let* ((constants-exps (get-prefix-from-parsed parsed `() 'const))
			(const-subs (flatten (map subsets-for-symbols constants-exps)))
			(only-symbols (filter symbol? const-subs))
			(remove-duplicated (remove-dups only-symbols))
			(complete-symbol-table (map (lambda (x) `(,x ,(lookup-table constant-table x) ,(lookup-table constant-table (symbol->string x)))) remove-duplicated)))
			  complete-symbol-table 
			)
		)
	)

(define number-list
	(lambda (lst container i)
		(if (null? lst)
			(reverse container)
			(number-list (cdr lst) (cons `(,i ,(car lst)) container) (+ i 1))
			)
		)
	)

(define build-fvar-table ;TODO: add more core-funcs here
	(lambda (parsed)
		(let* ((default-fvars `(cons car cdr append apply < > = greater-than-amir less-than-amir equal-binary-amir onary_minus binary_div binary_mul binary_plus binary_minus / * - boolean? char->integer char? cons denominator eq? integer? integer->char list make-string make-vector map not null?
								number? numerator pair? procedure? rational? remainder set-car! set-cdr! string-length string-ref string-set! string->symbol string? symbol? symbol->string vector vector-length vector-ref vector-set! vector? zero?))
			(fvar-exps (get-prefix-from-parsed parsed `() 'fvar))
			(all-fvar-values (append default-fvars fvar-exps))
			(remove-duplicated (remove-dups all-fvar-values))
			(constant-table-length-in-mem (fold-left (lambda (acc x) (+ acc (- (length x) 2))) 0 constant-table))
			(numbered-fvars (number-list remove-duplicated '() (+ 2 constant-table-length-in-mem))))
			numbered-fvars
			)
		)
	)

(define constant-table 'moshe)
(define fvar-table 'yossi)
(define symbol-table 'haim)


(define const->memory
	(lambda (x)
         (cond ((equal? (caddr x) "T_VOID")  "CALL(MAKE_SOB_VOID);\n")
               ((equal? (caddr x) "T_NIL")  "CALL(MAKE_SOB_NIL);\n")
               ((equal? (caddr x) "T_BOOL")  (string-append "PUSH(" (if (= (cadddr x) 0 ) "0" "1") ");\n" "CALL(MAKE_SOB_BOOL);\n" "DROP(1);\n" ))
               ((equal? (caddr x) "T_INTEGER")  (string-append "PUSH(" (number->string(cadddr x)) ");\n" "CALL(MAKE_SOB_INTEGER);\n" "DROP(1);\n" ))
               ((equal? (caddr x) "T_CHAR")  (string-append "PUSH(" (number->string(cadddr x)) ");\n" "CALL(MAKE_SOB_CHAR);\n" "DROP(1);\n" ))
               ((equal? (caddr x) "T_PAIR")  (string-append "PUSH(" (number->string (car (cddddr x))) ");\n" "PUSH(" (number->string(cadddr x)) ");\n" "CALL(MAKE_SOB_PAIR);\n" "DROP(2);\n" ))
               ((equal? (caddr x) "T_STRING")  (apply string-append `(,@(map (lambda(x)  (string-append "PUSH(" (number->string x) ");\n") )   (cddddr x))  ,"PUSH(" ,(number->string (cadddr x)) ,");\n" "CALL(MAKE_SOB_STRING);\n"  ,"DROP(",(number->string (car (cdddr x)) ) ,");\n" )))
               ((equal? (caddr x) "T_VECTOR")  (apply string-append `(,@(map (lambda(x)  (string-append "PUSH(" (number->string x) ");\n") )   (cddddr x))  ,"PUSH(" ,(number->string (cadddr x)) ,");\n" "CALL(MAKE_SOB_VECTOR);\n"  ,"DROP(",(number->string (car (cdddr x)) ) ,");\n" )))
               ((equal? (caddr x) "T_SYMBOL")  (string-append "PUSH(" (number->string(cadddr x)) ");\n" "CALL(MAKE_SOB_SYMBOL);\n" "DROP(1);\n"))
               ((equal? (caddr x) "T_FRACTION")  (string-append "PUSH(" (number->string (car(cddddr x))) ");\n" "PUSH(" (number->string (cadddr x)) ");\n" "CALL(MAKE_SOB_FRACTION);\n" "DROP(2);\n"))
               (else "ERROR in const->memory\n"))
         ))

(define symbol->memory
	(lambda (x)
		(let ((string-address (number->string (caddr x)))
			  (symbol-address (number->string (cadr x))))
		(string-append
			"MOV(INDD(" symbol-address ",1)," string-address ");\n"
			"PUSH(2);\n"
			"CALL(MALLOC);\n"
			"DROP(1);\n"
			"MOV(INDD(R0,0)," string-address ");\n"
			"MOV(INDD(R0,1),IND(1));\n"
			"MOV(IND(1),R0);\n"
		)))
	)



(define apply-ass3
	(lambda (sexpr)
		(annotate-tc
			(pe->lex-pe
				(box-set
					(remove-applic-lambda-nil
						(eliminate-nested-defines
							(parse sexpr))))))))



(define func-builder
	(lambda (sym label)  
		( string-append     
				"//" label "\n"
				"PUSH(LABEL(" label "));\n"
				"PUSH(-1); //env\n"
				"CALL(MAKE_SOB_CLOSURE);\n"
				"DROP(2);\n"
				"MOV(IND(" (number->string (lookup-table fvar-table sym)) "),R0);\n"
	))
)


(define add-lib
	(lambda (list-of-sexpr)

		`(
			(define cadr (lambda (t) (car (cdr t))))
			(define cdar (lambda (t) (cdr (car t))))
			(define cddr (lambda (t) (cdr (cdr t))))
			(define caar (lambda (t) (car (car t))))
			(define cadar (lambda (t) (car (cdr (car t)))))
			(define cdaar (lambda (t) (cdr (car (car t)))))
			(define cddar (lambda (t) (cdr (cdr (car t)))))
			(define caaar (lambda (t) (car (car (car t)))))
			(define caddr (lambda (t) (car (cdr (cdr t)))))
			(define cdadr (lambda (t) (cdr (car (cdr t)))))
			(define cdddr (lambda (t) (cdr (cdr (cdr t)))))
			(define caadr (lambda (t) (car (car (cdr t)))))
			(define cadaar (lambda (t) (car (cdr (car (car t))))))
			(define cdaaar (lambda (t) (cdr (car (car (car t))))))
			(define cddaar (lambda (t) (cdr (cdr (car (car t))))))
			(define caaaar (lambda (t) (car (car (car (car t))))))
			(define caddar (lambda (t) (car (cdr (cdr (car t))))))
			(define cdadar (lambda (t) (cdr (car (cdr (car t))))))
			(define cdddar (lambda (t) (cdr (cdr (cdr (car t))))))
			(define caadar (lambda (t) (car (car (cdr (car t))))))
			(define cadadr (lambda (t) (car (cdr (car (cdr t))))))
			(define cdaadr (lambda (t) (cdr (car (car (cdr t))))))
			(define cddadr (lambda (t) (cdr (cdr (car (cdr t))))))
			(define caaadr (lambda (t) (car (car (car (cdr t))))))
			(define cadddr (lambda (t) (car (cdr (cdr (cdr t))))))
			(define cdaddr (lambda (t) (cdr (car (cdr (cdr t))))))
			(define cddddr (lambda (t) (cdr (cdr (cdr (cdr t))))))
			(define caaddr (lambda (t) (car (car (cdr (cdr t))))))


			(define map (lambda (func lst)  (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst))))))
			
			(define foldl (lambda (func accum lst)
								  (if (null? lst)
								      accum
								      (foldl func
								             (func accum (car lst)) 
								             (cdr lst)))))

			(define append_binary  (lambda (l m)
 										(if (null? l) m
													  (cons (car l) (append_binary (cdr l) m)))))


			(define append (lambda s (if (= (list-length s) 1) (car s) (if (= (list-length s) 0) '() (foldl (lambda (xs x)  (append_binary xs x) ) '() s)))))


			(define list (lambda s s))

			
 
			(define =
				(lambda s (if (or (equal-binary-amir (list-length s) 1)(equal-binary-amir (list-length s) 0)) #t (and (equal-binary-amir (car s) (car (cdr s))) (apply = (cdr s)))))
			)

			(define list-length 
				(let ((binary_plus binary_plus)
					(null? null?))
					(lambda (lst)  (if (null? lst) 0 (binary_plus 1 (list-length (cdr lst))))) ; not tc-applic (is not YAIL - i dont care right now)
			))

			(define + 
				(let ((binary_plus binary_plus)
					  (list-length list-length)
					  (foldl foldl)
					  (= =)
					  (append append))
				(lambda s (if (= (list-length s) 1) (car s) (foldl binary_plus 0 (append s '(0))))))
			)

			(define -
				(let ((binary_minus binary_minus)
					  (list-length list-length)
					  (foldl foldl)
					  (= =)
					  (append append))
				(lambda s (if (= (list-length s) 1) (onary_minus (car s)) (if (= (list-length s) 0) 0 (foldl binary_minus (car s) (cdr s))))))
			)

			(define *
				(let ((binary_mul binary_mul)
					  (list-length list-length)
					  (foldl foldl)
					  (= =)
					  (append append))
				(lambda s (if (= (list-length s) 1) (car s) (if (= (list-length s) 0) 1 (foldl binary_mul 1 s )))))
			)

			(define /
				(let ((binary_div binary_div)
					  (list-length list-length)
					  (foldl foldl)
					  (= =)
					  (append append))
				(lambda s (if (= (list-length s) 1) (binary_div 1 (car s)) (if (= (list-length s) 0) 1 (foldl binary_div (car s) (cdr s))))))
			)


			(define <
				(lambda s (if (or (equal-binary-amir (list-length s) 1)(equal-binary-amir (list-length s) 0)) #t (and (less-than-amir (car s) (car (cdr s))) (apply < (cdr s)))))
			)
			(define >
				(lambda s (if (or (equal-binary-amir (list-length s) 1)(equal-binary-amir (list-length s) 0)) #t (and (greater-than-amir (car s) (car (cdr s))) (apply > (cdr s)))))
			)




			,@list-of-sexpr)
	)
) 

(define compile-scheme-file 
	(lambda (file output) 
		(let* (
			(strings (string->sexprs (file->string file)))
			(parsed (map apply-ass3 (add-lib strings)))
    	  (initialize-const-table (begin  (set! constant-table (build-constants-table parsed))
    	   								   (apply string-append (cons "\n//initialize constant-table\n" (map const->memory constant-table)))
    	   							))
    	   (initialize-symbol-table (begin (set! symbol-table (build-symbol-table parsed)) ;;uses constant table
    	   								(string-append
    	   									(apply string-append (cons "\n//initialize symbol-table\n" (map symbol->memory symbol-table)))
    	   								)))
    	   (initialize-fvar-table  (begin (set! fvar-table (build-fvar-table parsed))
    	   								(string-append
    	   										"\n//initialize fvar-table\n"
    	   										"PUSH(" (number->string (length fvar-table)) ");\n"
    	   										"CALL(MALLOC);\n"
    	   										"DROP(1);\n"
    	   										(apply string-append (map (lambda (x) (string-append "MOV(IND(" (number->string (car x)) "),-1);\n")) fvar-table))
    	   										(func-builder 'car "CAR")
												(func-builder 'cdr "CDR")
												(func-builder 'cons "CONS")
												 (func-builder 'less-than-amir "LESS_THAN_BINARY")
												 (func-builder 'binary_plus "BINARY_PLUS")
												 (func-builder 'onary_minus "ONARY_MINUS")
												 (func-builder 'equal-binary-amir "EQUAL_BINARY")	
												(func-builder 'greater-than-amir "GREATER_THAN_BINARY")
												 (func-builder 'binary_div "BINARY_DIV")
												 (func-builder 'binary_mul "BINARY_MUL")
												 (func-builder 'binary_minus "BINARY_MINUS")
												 (func-builder 'boolean? "BOOLEAN_PREDIC")
												 (func-builder 'apply "APPLY")
												 (func-builder 'char->integer "CHAR_TO_INTEGER")
												 (func-builder 'char? "CHAR_PREDIC")
												 (func-builder 'denominator "DENOMINATOR")
												 (func-builder 'eq? "EQ")
												 (func-builder 'integer? "INTEGER_PREDIC")
												 (func-builder 'integer->char  "INTEGER_TO_CHAR")
												 (func-builder 'make-string "MAKE_STRING")
												 (func-builder 'make-vector "MAKE_VECTOR")
												 (func-builder 'not "NOT")
												 (func-builder 'null? "NULL_PREDIC")
												 (func-builder 'number? "NUMBER_PREDIC")
												 (func-builder 'numerator  "NUMERATOR")
												 (func-builder 'pair?  "PAIR_PREDIC")
												 (func-builder 'procedure? "PROCEDURE_PREDIC")
												 (func-builder 'rational? "RATIONAL_PREDIC")
												 (func-builder 'remainder "REMAINDER")
												 (func-builder 'set-car! "SET_CAR")
												 (func-builder 'set-cdr! "SET_CDR")
												 (func-builder 'string-length "STRING_LENGTH")
												 (func-builder 'string-ref "STRING_REF")
												 (func-builder 'string-set!  "STRING_SET")
												 (func-builder 'string->symbol "STRING_TO_SYMBOL")
												 (func-builder 'string? "STRING_PREDIC")
												 (func-builder 'symbol?  "SYMBOL_PREDIC")
												 (func-builder 'symbol->string  "SYMBOL_TO_STRING")
												 (func-builder 'vector  "VECTOR")
												 (func-builder 'vector-length  "VECTOR_LENGTH")
												 (func-builder 'vector-ref   "VECTOR_REF")
												 (func-builder 'vector-set!   "VECTOR_SET")
												 (func-builder 'vector?   "VECTOR_PREDIC")
												 (func-builder 'zero?  "ZERO_PREDIC")
												)
    	   							))
    	   	(data_section (string-append
				"#include \"char.lib\"\n"
				"#include \"io.lib\"\n"
				"#include \"math.lib\"\n"
				"#include \"string.lib\"\n"
				"#include \"system.lib\"\n"
				"#include \"scheme.lib\"\n"
				"#include \"mylib.asm\"\n"
				)
			)
    	   (generated-code (map (lambda (e) (string-append (code-gen e 0)  	
    	   																	"PUSH(R0);\n"
    	   																	"CALL(WRITE_SOB_WARPPER);\n"
																			"DROP(1);\n"
																			 )) parsed)) ; AMIR :: added print for each parse !
    	   (to-write (apply string-append `(	
    	   	"#include <stdio.h>\n"
    	   	"#include <stdlib.h>\n"
    	   	"#define DO_SHOW 1\n\n"
    	   	"#include \"cisc.h\"\n"
    	   	"#include \"debugger.h\"\n\n"
    	   	"#define SOB_VOID 2\n"
			"#define SOB_NIL 3\n"
			"#define SOB_TRUE 4\n"
			"#define SOB_FALSE 6\n\n"

    	   	"int main(){\n\n"
	    	   	"START_MACHINE;\n\n"
	    	   	"JUMP(CONTINUE);\n\n"
	    	   	,data_section
  				"\nCONTINUE:\n"
	    	   	"PUSH(1); //make room for symbol-table\n"
				"CALL(MALLOC);\n"
				"DROP(1);\n"
				"MOV(IND(R0),IMM(SOB_NIL));\n"
				,initialize-const-table
	    	   	,initialize-fvar-table
	    		,initialize-symbol-table
	    	   	"\n//code-generation\n"
	    	   	;"INFO;\n"
	    	   	,@generated-code
	    	   	;"INFO;\n"
	    	   	"L_error_cannot_apply_non_clos:\n"
	    	   	"L_error_lambda_args_count:\n"
	    	   	"L_error_incorr_type:\n"
	    	   	"//INFO; //BEN EYAL's DEBUGGER\n\n"
	    	   	"\nSTOP_MACHINE;\n\n"
	    	   	"return 0;\n"
    	   	"}"
    	   	))))
      		;(string->file to-write) ;DO NOT REMOVE
      		(string->file to-write output) ;REMOVE LATER
      		)
		)
	)

(define string->file
	(lambda (string file)
		(delete-file file)
		(let ((out-port (open-output-file file)))
			(letrec ((run (lambda (lst)
				(if (null? lst)
					(close-output-port out-port)
					(begin
						(write-char (car lst) out-port)
						(run (cdr lst)))))))
			(run (string->list string))
			))))



(define index 0)

(define index++ (lambda () (set! index (+ 1 index))))

(define print-index (lambda () (number->string index)))

(define get-last
	(lambda (lst)
		(car (reverse lst))
		)
	)

(define lose-last
	(lambda (lst)
		(reverse (cdr (reverse lst))))
	)

(define vectors-copy
	(lambda (i j major s)
		(if (not (< i major))
			s
			(vectors-copy (+ 1 i) (+ 1 j) major
				(string-append s "MOV(INDD(R2," (number->string j) "),INDD(R1," (number->string i) "));\n"))
			)
		)
	)


(define make-closure
	(lambda (major params current-num)
		(string-append
			"MOV(R1,FPARG(0)); //env\n"
	  		"PUSH(" (number->string (+ 1 major)) ");\n"
	  		"CALL(MALLOC);\n"
	  		"DROP(1);\n"
	  		"MOV(R2,R0);\n"
			(vectors-copy 0 1 major "") ;; call with major?
			"MOV(R3,FPARG(1)); //n\n"
			"PUSH(R3);\n"
			"CALL(MALLOC);\n"
			"DROP(1);\n"
			"MOV(INDD(R2,0),R0);\n"

			"MOV(R8,0);\n" ; R8 => i
 			"MOV(R9,2);\n" ; R9 => j
			"LEANSHEU_"current-num":\n"
			"CMP(R8,R3);\n"
			"JUMP_GE(EXIT_LEANSHEU_"current-num");\n"
			"MOV(INDD(INDD(R2,0),R8),FPARG(R9));\n"
			"INCR(R9);\n"
			"INCR(R8);\n"
			"JUMP(LEANSHEU_"current-num");\n"
			"EXIT_LEANSHEU_"current-num":\n"

			"PUSH(LABEL(L_clos_body_" current-num "));\n"
			"PUSH(R2); //env\n"
			"CALL(MAKE_SOB_CLOSURE);\n"
			"DROP(2);\n"
			"JUMP(L_clos_exit_" current-num ");\n"
			)
		)
	)

(define code-gen
	(lambda (ast major)
		;(display ast)
		(cond ((ass3-const? ast) (string-append "MOV(R0,IMM(" (number->string (lookup-table constant-table (cadr ast))) "));\n"))
			((if? ast) (begin (index++)
				(let ((current-num (print-index)))
					(string-append (code-gen (cadr ast) major)
						"CMP(R0,IMM(SOB_FALSE));\n"
						"JUMP_EQ(L_if3_else_" current-num ");\n"
						(code-gen (caddr ast) major)
						"JUMP(L_if3_exit_" current-num ");\n"
						"L_if3_else_" current-num ":\n"
						(code-gen (cadddr ast) major)
						"L_if3_exit_" current-num ":\n"))
				))
			((seq? ast) (apply string-append (map (lambda (e) (code-gen e major)) (cadr ast))))
			((or? ast) (begin (index++)
				(let* ((current-num (print-index))
					(all-but-last (map (lambda (e) `(,(code-gen e major)
						"CMP(R0,IMM(SOB_FALSE));\n"
						"JUMP_NE(L_or_exit_" ,current-num ");\n"))
					(lose-last (cadr ast)))))
				(apply string-append `(,@(flatten all-but-last)
					,(code-gen (get-last (cadr ast)) major)
					"L_or_exit_" ,current-num ":\n")))
				))
			((applic? ast) (let ((args (reverse (caddr ast)))
				(proc (cadr ast)))
			(string-append
				(apply string-append (map (lambda (b) (string-append (code-gen b major) "PUSH(R0);\n")) args))
				"PUSH(" (number->string (length args)) ");\n"
				(code-gen proc major)
				"CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
				"JUMP_NE(L_error_cannot_apply_non_clos);\n"
				"PUSH(INDD(R0,1)); //env\n"
				"CALLA(INDD(R0,2));\n"
				"DROP(1);\n"
				"POP(R1);\n"
				"DROP(R1);\n"
				)))
		((equal? (car ast) 'tc-applic) (begin (index++)
										(let ((args (reverse (caddr ast)))
			    							(proc (cadr ast))
			    							(current-num (print-index)))
										 (string-append
										 	(apply string-append (map (lambda (b) (string-append (code-gen b major) "PUSH(R0);\n")) args))
										 	"PUSH(IMM(" (number->string (length args)) "));\n"
										 	(code-gen proc major)
										 	"CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
										 	"JUMP_NE(L_error_cannot_apply_non_clos);\n"
											"PUSH(INDD(R0,IMM(1))); //env\n"
										 	"PUSH(FPARG(-1)); //old ret\n"

										 	"MOV(R3,FP);\n"
											"SUB(R3,FPARG(-2));\n"
											"MOV(R4,IMM(" (number->string (length args)) "));\n"
											"ADD(R4,3); //number of total elements on stack\n"
											"MOV(R5,R4); //save for later\n"
											"MOV(R1,IMM(0)); //counter\n"
											"MOV(R6,FP);"
								
											"MOV(R10,FP);\n"
											"SUB(R10,FPARG(1)); //new framepointer\n"
											"SUB(R10,4);\n"
											"MOV(R11,FPARG(-2));\n"
											"MOV(FP,R10);\n"

											"L_tc_applic_" current-num ":\n"
											"CMP(R1,R4);\n"
											"JUMP_EQ(L_tc_end_" current-num ");\n"
											"MOV(R7,FP);\n"
											"ADD(R7,R1);\n"
											"MOV(R8,R6);\n"
											"ADD(R8,R1);\n"
											"MOV(STACK(R7),STACK(R8));\n"
											"INCR(R1);\n"
											"JUMP(L_tc_applic_" current-num ");\n"
											"L_tc_end_" current-num ":\n"
											"MOV(SP,FP);\n"
											"ADD(SP,R5);\n"
											"MOV(FP,R11);\n"

											"JUMPA(INDD(R0,IMM(2)));\n"
										 	)))
   	 	  	)
    	  ((equal? (car ast) 'pvar) (string-append "MOV(R0,FPARG(" (number->string (+ 2 (caddr ast))) "));\n"))
    	  ((equal? (car ast) 'bvar) (string-append "// BVAR " (symbol->string (cadr ast)) "\n"
    	  										   "MOV(R0,FPARG(0)); //env\n"
    	  							  			   "MOV(R0,INDD(R0," (number->string (caddr ast)) "));\n"
    	  							  			   "MOV(R0,INDD(R0," (number->string (cadddr ast)) "));\n"
    	  							  			   "// END-BVAR " (symbol->string (cadr ast)) "\n"
    	  								))
    	  ;;TODO: check if works
    	  ((equal? (car ast) 'set) (cond ((equal? (caadr ast) 'pvar) (string-append (code-gen (caddr ast) major)
    	  																			"MOV(FPARG(" (number->string (+ 2 (caddr (cadr ast)))) "),R0);\n"
    	  																			"MOV(R0,IMM(SOB_VOID));\n"))
    	  								 ((equal? (caadr ast) 'fvar) (string-append (code-gen (caddr ast) major)
    	  																			"MOV(IND(" (number->string (lookup-table fvar-table (cadadr ast))) "),R0);\n"
    	  																			"MOV(R0,IMM(SOB_VOID));\n"))
    	  								 ((equal? (caadr ast) 'bvar) (string-append (code-gen (caddr ast) major)
    	  								 											"MOV(R1,FPARG(0)); //env\n"
									    	  							  			"MOV(R1,INDD(R1," (number->string (caddr (cadr ast))) "));\n"
 									    	  							  			"MOV(INDD(R1," (number->string (cadddr (cadr ast))) "),R0);\n"
 									    	  							  			"MOV(R0,IMM(SOB_VOID));\n"
 									    	  							  			))
			
			))
    	  ;;TODO: check if works
    	  ((equal? (car ast) 'box-get) (cond ((equal? (caadr ast) 'pvar) (string-append "MOV(R0,FPARG(" (number->string (+ 2 (caddr (cadr ast)))) "));\n"
   	  																					"MOV(R0,IND(R0)); //unbox\n"))
     	  									 ((equal? (caadr ast) 'fvar) (string-append "MOV(R0,IND(" (number->string (lookup-table fvar-table (cadr (cadr ast)))) "));\n"
    	  																				"MOV(R0,IND(R0)); //unbox\n"))
     	  									 ((equal? (caadr ast) 'bvar) (string-append "MOV(R0,FPARG(0)); //env\n"
     	  							  			   										"MOV(R0,INDD(R0," (number->string (caddr (cadr ast))) "));\n"
    	  							  			   										"MOV(R0,INDD(R0," (number->string (cadddr (cadr ast))) "));\n"
    	  																				"MOV(R0,IND(R0)); //unbox\n"))
    	  
			
			))
    	   ((equal? (car ast) 'box) (cond ((equal? (caadr ast) 'pvar) (string-append "PUSH(1);\n"
   	  																			"CALL(MALLOC);\n"
    	  																			"DROP(1);\n"
    	  																			"MOV(IND(R0),FPARG(" (number->string (+ 2 (caddr (cadr ast)))) "));\n"))
     	  								 ((equal? (caadr ast) 'fvar) (string-append "MOV(R0,IND(" (number->string (lookup-table fvar-table (cadr (cadr ast)))) "));\n"))
    	  								 ((equal? (caadr ast) 'bvar) (string-append "MOV(R1,FPARG(0)); //env\n"
   	  							  			   									"MOV(R1,INDD(R1," (number->string (caddr (cadr ast))) "));\n"
     	  							  			   									"MOV(R0,INDD(R1," (number->string (cadddr (cadr ast))) "));\n"))
     	  	))
   	 	 	((equal? (car ast) 'box-set) (cond ((equal? (caadr ast) 'pvar) (string-append (code-gen (caddr ast) major)
      																				"MOV(R1,FPARG(" (number->string (+ 2 (caddr (cadr ast)))) "));\n"
      																				"MOV(IND(R1),R0);\n"
    	  																				"MOV(R0,IMM(SOB_VOID));\n"))
	    	  								 ((equal? (caadr ast) 'fvar) (string-append (code-gen (caddr ast) major)
	    	  																			"MOV(R1,IND(" (number->string (lookup-table fvar-table (cadadr ast))) "));\n"
    	  																			"MOV(IND(R1),R0);\n"
    	  																			"MOV(R0,IMM(SOB_VOID));\n"))
    	  								 ((equal? (caadr ast) 'bvar) (string-append (code-gen (caddr ast) major)
    	  								 											"MOV(R1,FPARG(0)); //env\n"
									    	  							  			"MOV(R1,INDD(R1," (number->string (caddr (cadr ast))) "));\n"
									    	  							  			"MOV(R1,INDD(R1," (number->string (cadddr (cadr ast))) "));\n"
									    	  							  			"MOV(IND(R1),R0);\n"
									    	  							  			"MOV(R0,IMM(SOB_VOID));\n"
									    	  							  			))
    	  								 (display 'ho)

   	  		))
    	  ((equal? (car ast) 'fvar) (string-append "MOV(R0,IND(" (number->string (lookup-table fvar-table (cadr ast))) "));\n"))
    	  ((equal? (car ast) 'def) (string-append
    	  								"// DEFINE " (symbol->string(cadadr ast)) " \n"
    	  								(code-gen (caddr ast) major)
    	  						   		"MOV(IND(" (number->string (lookup-table fvar-table (cadadr ast))) "),R0);\n"
    	  						   		"MOV(R0,IMM(SOB_VOID));\n"
    	  						   		"// END-DEFINE " (symbol->string(cadadr ast)) " \n"
    	  	))

    	  ((equal? (car ast) 'lambda-simple) (begin  (index++)
    	  	(let ((current-num (print-index))
    	  		(params (get-params ast))
    	  		(body (caddr ast)))
    	  	(string-append
    	  		(make-closure major params current-num)
				"L_clos_body_" current-num ":\n"
				"PUSH(FP);\n"
				"MOV(FP,SP);\n"
				"CMP(FPARG(1),IMM(" (number->string (length params)) "));\n"
				"JUMP_NE(L_error_lambda_args_count);\n"
				(code-gen body (+ 1 major))
				"POP(FP);\n"
				"RETURN;\n"
				"L_clos_exit_" current-num ":\n"
				)
    	  	)))
    	  ((equal? (car ast) 'lambda-opt)(begin  (index++)
    	  	(let ((current-num (print-index))
    	  		(params (get-params ast))
    	  		(body (cddr ast)))
    	  	(string-append
    	  		(make-closure major params current-num)
				"L_clos_body_" current-num ":\n"
				"PUSH(FP);\n"
				"MOV(FP,SP);\n"


				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;



				; checking correct numbers of args
				"// We got enough args ?"
				"CMP(FPARG(1),IMM(" (number->string (- (length params) 1)) "));\n"
				"JUMP_LT(L_error_lambda_args_count);\n\n"


				; SOME CALCULATIONS
				"//CALCULATIONS - look at the doc.\n"
				"MOV(R4,FPARG(1)); // R4 = SIZE OF ACTUAL ARGS SENT\n"
				"MOV(R5,IMM(" (number->string (- (length params) 1)) ")); //  R5 = PVAR LENGTH \n"
				"MOV(R6,R4);\n"		
				"SUB(R6,R5); //  R6 = ARGS-PARAMS = REST \n\n"

			
				; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
				; ~~~~~~~============================= FIXING STACK SECTION ===============================~~~~~~~
				; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n"
				"// =-=-=-=-=-=-=-=-=-=-=  FIXING STACK SECTION - OPT LAMBDA - =-=-=-=-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n"

		

				"// Saving 4 last stack info.\n"
				"POP(R10); // R10 = oldfp \n" ; oldfp
				"POP(R11); // R11 = ret \n" ; ret
				"POP(R12); // R12 = env \n" ; env
				"POP(R13); // R13 = size w/o rest \n\n" ; size
				
		
				"// R7 Will be now pointing at the start of the args. (Bn) \n"
				"MOV(R7,SP); \n"
				"SUB(R7,R4); \n\n"

				"// Making Rest base struct and saving it at R9\n"
				"MOV(R0,SOB_NIL);// NIL ADDRESS\n" 
				"MOV(R9,R0);\n"

	


				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bi
				;	R9 = REST STRUCT ADDRESS

				"MOV(R2,R6);\n"

				"OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num ":\n"
				"CMP(R2,IMM(0));\n"
				"JUMP_EQ(END_OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num");\n"	
				"PUSH(R9);\n"
				"PUSH(STACK(R7));\n"
				"CALL(MAKE_SOB_PAIR);\n"
				"DROP(2);\n"
				"MOV(R9,R0);\n"
				"INCR(R7);\n"
				"DECR(R2);\n"
				"JUMP(OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num");\n"

				"END_OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num":\n"

				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=\n"
				"// =-=-= FIXING STACK SECTION  -  SHIFTING (v1 or v2)  - OPT LAMBDA - =-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=\n\n"

				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.


				"// R7 Will be now pointing at the start of the args. (Bn) \n"
				"MOV(R7,SP); \n"
				"SUB(R7,R13); \n\n"


				"// Checks maybe its a MIKRE KATZE\n"
				"CMP(R4,R5);\n"
				"JUMP_EQ(MIKRE_KATZE_OPT_"current-num");\n"

				"// =-=-=-=-=-==-=-=-=-=-=-=-=-=-=---=-=-=-=-=\n"
				"// =-=-=  NORMAL SHIFTING - =-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=\n\n"

				; 				DOC
				;	R2 = POINTING SRC COPY
				;	R3 = POINTING DST COPY
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.
				;	R15= SP+1

				"// Put the Rest in his address & update R3 \n"
				"MOV(STACK(R7),R9);\n"
				"MOV(R3,R7);\n"
				"INCR(R3);\n\n"

				"// Initialize R15 as doc said\n"
				"MOV(R15,SP);\n"
				"INCR(R15);\n"

				"// Initialize R2 as doc said \n"
				"MOV(R2,SP);\n"
				"SUB(R2,R5);\n\n"

				"// loop for normal shifting\n"
				"OPT_SHIFTING_"current-num ":\n"
				"CMP(R2,R15);\n"
				"JUMP_EQ(END_OPT_SHIFTING_"current-num");\n"
				"INCR(R7);\n"
				"MOV(STACK(R3),STACK(R2));\n"
				"INCR(R2);\n"
				"INCR(R3);\n"

				"JUMP(OPT_SHIFTING_"current-num ");\n"


				"END_OPT_SHIFTING_"current-num ":\n"
				"MOV(SP,R7);\n"			


				"JUMP(END_OPT_"current-num");\n\n"


				"// =-=-=-=-=-==-=-=-=-=-=-=-=-=-=---=-=-=-=-=\n"
				"// =-=-=  MIKRE_KATZE_OPT - =-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=\n\n"

				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.
				;	R15 = SP

				"MIKRE_KATZE_OPT_"current-num":\n"
				
					
				"MOV(R15,SP);\n"
				
				"MOV(R14,SP);\n"
				"DECR(R14);\n"
				"INCR(SP);\n"

				"MOV(R3,R5);\n"



				"MIKRE_KATZE_OPT_LOOP_"current-num":"
				"CMP(R3,IMM(0));\n"
			
				"JUMP_EQ(LAST_THING_OPT_MIKRE_KATZE_"current-num")\n"
				"MOV(STACK(R15),STACK(R14));\n"
				"DECR(R15);\n"
				"DECR(R14);\n"
				"DECR(R3);\n"
				"JUMP(MIKRE_KATZE_OPT_LOOP_"current-num");\n"

				"LAST_THING_OPT_MIKRE_KATZE_"current-num":"
				"MOV(STACK(R15),R9);\n"


				"END_OPT_"current-num":"

				"PUSH(" (number->string (length params) )");\n"	
				"PUSH(R12);\n"
				"PUSH(R11);\n"
				"PUSH(R10);\n"
				"MOV(FP,SP);\n"

				;"INFO;"
				;"INFO;"
				(apply string-append (map (lambda (x) (code-gen x (+ 1 major))) (cdr body)))
				;"INFO;"
				
				"POP(FP);\n" 
				"RETURN;\n"
				"L_clos_exit_" current-num ":\n"
				)
    	  	)))
    	  ((equal? (car ast) 'lambda-var)(begin  (index++)
    	  	(let ((current-num (print-index))
    	  		(params (get-params ast))
    	  		(body (cddr ast)))
    	  	(string-append
    	  		(make-closure major params current-num)
				"L_clos_body_" current-num ":\n"
				"PUSH(FP);\n"
				"MOV(FP,SP);\n"


				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;





				; SOME CALCULATIONS
				"//CALCULATIONS - look at the doc.\n"
				"MOV(R4,FPARG(1)); // R4 = SIZE OF ACTUAL ARGS SENT\n"
				"MOV(R5,IMM(0)); //  R5 = PVAR LENGTH \n"
				"MOV(R6,R4);\n"		
				"SUB(R6,R5); //  R6 = ARGS-PARAMS = REST \n\n"

			
				; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
				; ~~~~~~~============================= FIXING STACK SECTION ===============================~~~~~~~
				; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n"
				"// =-=-=-=-=-=-=-=-=-=-=  FIXING STACK SECTION - OPT LAMBDA - =-=-=-=-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n"

		

				"// Saving 4 last stack info.\n"
				"POP(R10); // R10 = oldfp \n" ; oldfp
				"POP(R11); // R11 = ret \n" ; ret
				"POP(R12); // R12 = env \n" ; env
				"POP(R13); // R13 = size w/o rest \n\n" ; size
				
		
				"// R7 Will be now pointing at the start of the args. (Bn) \n"
				"MOV(R7,SP); \n"
				"SUB(R7,R4); \n\n"

				"// Making Rest base struct and saving it at R9\n"
				"MOV(R0,SOB_NIL);// NIL ADDRESS\n" 
				"MOV(R9,R0);\n"

	


				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bi
				;	R9 = REST STRUCT ADDRESS

				"MOV(R2,R6);\n"

				"OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num ":\n"
				"CMP(R2,IMM(0));\n"
				"JUMP_EQ(END_OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num");\n"	
				"PUSH(R9);\n"
				"PUSH(STACK(R7));\n"
				"CALL(MAKE_SOB_PAIR);\n"
				"DROP(2);\n"
				"MOV(R9,R0);\n"
				"INCR(R7);\n"
				"DECR(R2);\n"
				"JUMP(OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num");\n"

				"END_OPT_LIST_MAKING_ARGS_PUSH_LOOP_"current-num":\n"

				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=\n"
				"// =-=-= FIXING STACK SECTION  -  SHIFTING (v1 or v2)  - OPT LAMBDA - =-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=\n\n"

				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.


				"// R7 Will be now pointing at the start of the args. (Bn) \n"
				"MOV(R7,SP); \n"
				"SUB(R7,R13); \n\n"


				"// Checks maybe its a MIKRE KATZE\n"
				"CMP(R4,R5);\n"
				"JUMP_EQ(MIKRE_KATZE_OPT_"current-num");\n"

				"// =-=-=-=-=-==-=-=-=-=-=-=-=-=-=---=-=-=-=-=\n"
				"// =-=-=  NORMAL SHIFTING - =-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=\n\n"

				; 				DOC
				;	R2 = POINTING SRC COPY
				;	R3 = POINTING DST COPY
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.
				;	R15= SP+1

				"// Put the Rest in his address & update R3 \n"
				"MOV(STACK(R7),R9);\n"
				"MOV(R3,R7);\n"
				"INCR(R3);\n\n"

				"// Initialize R15 as doc said\n"
				"MOV(R15,SP);\n"
				"INCR(R15);\n"

				"// Initialize R2 as doc said \n"
				"MOV(R2,SP);\n"
				"SUB(R2,R5);\n\n"

				"// loop for normal shifting\n"
				"OPT_SHIFTING_"current-num ":\n"
				"CMP(R2,R15);\n"
				"JUMP_EQ(END_OPT_SHIFTING_"current-num");\n"
				"INCR(R7);\n"
				"MOV(STACK(R3),STACK(R2));\n"
				"INCR(R2);\n"
				"INCR(R3);\n"

				"JUMP(OPT_SHIFTING_"current-num ");\n"


				"END_OPT_SHIFTING_"current-num ":\n"
				"MOV(SP,R7);\n"			


				"JUMP(END_OPT_"current-num");\n\n"


				"// =-=-=-=-=-==-=-=-=-=-=-=-=-=-=---=-=-=-=-=\n"
				"// =-=-=  MIKRE_KATZE_OPT - =-=-=-=-=-=-=-=-=-\n"
				"// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=\n\n"

				; 				DOC
				;	R4 = SIZE OF ACTUAL ARGS SENT - CONST.
				;	R5 = PVAR LENGTH / PARMS - CONST
				; 	R6 = ARGS-PARAMS = REST - CONST
				;	R10= oldfp
				;	R11= ret
				;	R12= env
				;	R13= size
				;	R7 = POINTING AT THE Bn
				;	R9 = REST STRUCT ADDRESS
				;	SP = SP at start minus 4 last infos.
				;	R15 = SP

				"MIKRE_KATZE_OPT_"current-num":\n"
				
					
				"MOV(R15,SP);\n"
				
				"MOV(R14,SP);\n"
				"DECR(R14);\n"
				"INCR(SP);\n"

				"MOV(R3,R5);\n"



				"MIKRE_KATZE_OPT_LOOP_"current-num":"
				"CMP(R3,IMM(0));\n"
			
				"JUMP_EQ(LAST_THING_OPT_MIKRE_KATZE_"current-num")\n"
				"MOV(STACK(R15),STACK(R14));\n"
				"DECR(R15);\n"
				"DECR(R14);\n"
				"DECR(R3);\n"
				"JUMP(MIKRE_KATZE_OPT_LOOP_"current-num");\n"

				"LAST_THING_OPT_MIKRE_KATZE_"current-num":"
				"MOV(STACK(R15),R9);\n"


				"END_OPT_"current-num":"

				"PUSH(" (number->string (length params) )");\n"	
				"PUSH(R12);\n"
				"PUSH(R11);\n"
				"PUSH(R10);\n"
				"MOV(FP,SP);\n"

				;"INFO;"
				;"INFO;"
				(apply string-append (map (lambda (x) (code-gen x (+ 1 major))) body))
				;"INFO;"
				;	"INFO;"
				"POP(FP);\n" 
				"RETURN;\n"
				"L_clos_exit_" current-num ":\n"
				)
    	  	)))
    	  (else "ERROR in code-gen\n")
    	  )
		)
	)