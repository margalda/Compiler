CAR:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1),IMM(1));
	JUMP_NE(L_error_lambda_args_count);
	MOV(R1,FPARG(2));
	CMP(INDD(R1,0),IMM(T_PAIR));
	JUMP_NE(L_error_incorr_type);
	MOV(R0,INDD(R1,1));
	POP(FP);
	RETURN;
	
CDR:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1),IMM(1));
	JUMP_NE(L_error_lambda_args_count);
	MOV(R1,FPARG(2));
	CMP(INDD(R1,0),IMM(T_PAIR));
	JUMP_NE(L_error_incorr_type);
	MOV(R0,INDD(R1,2));
	POP(FP);
	RETURN;

CONS:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(FPARG(3));
	PUSH(FPARG(2));
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	POP(FP);
	RETURN;


WRITE_SOB_WARPPER:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, FPARG(0));
  MOV(R0, IND(R0));
  CMP(R0, IMM(T_VOID));

  JUMP_EQ(IS_VOID_PRINTING);
  PUSH(FPARG(0));
 	CALL(WRITE_SOB);
  DROP(1);
  PUSH(R0);
 	CALL(NEWLINE);
 	POP(R0);
  IS_VOID_PRINTING:
	POP(FP);
 	RETURN;

BOOLEAN_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_BOOL);
  JUMP_EQ(BOOLEAN_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(BOOLEAN_PREDIC_EXIT);
BOOLEAN_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
BOOLEAN_PREDIC_EXIT:
  POP(FP);
  RETURN;

INTEGER_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(INTEGER_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(INTEGER_PREDIC_EXIT);
INTEGER_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
INTEGER_PREDIC_EXIT:
  POP(FP);
  RETURN;

NULL_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_NIL);
  JUMP_EQ(NULL_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(NULL_PREDIC_EXIT);
NULL_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
NULL_PREDIC_EXIT:
  POP(FP);
  RETURN;

PAIR_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_PAIR);
  JUMP_EQ(PAIR_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(PAIR_PREDIC_EXIT);
PAIR_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
PAIR_PREDIC_EXIT:
  POP(FP);
  RETURN;


STRING_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_STRING);
  JUMP_EQ(STRING_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(STRING_PREDIC_EXIT);
STRING_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
STRING_PREDIC_EXIT:
  POP(FP);
  RETURN;

VECTOR_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_VECTOR);
  JUMP_EQ(VECTOR_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(VECTOR_PREDIC_EXIT);
VECTOR_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
VECTOR_PREDIC_EXIT:
  POP(FP);
  RETURN;

//;#define T_VOID 937610
//;#define T_NIL 722689 
//;#define T_BOOL 741553
//;#define T_CHAR 181048
//;#define T_INTEGER 945311 
//;#define T_STRING 799345
//;#define T_SYMBOL 368031
//;#define T_PAIR 885397
//;#define T_VECTOR 335728
//;#define T_CLOSURE 276405
//;#define T_FRACTION 922870


CHAR_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_CHAR);
  JUMP_EQ(CHAR_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(CHAR_PREDIC_EXIT);
CHAR_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
CHAR_PREDIC_EXIT:
  POP(FP);
  RETURN;



NOT:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R1,FPARG(2));
  	CMP(R1, SOB_FALSE);
  	JUMP_EQ(NOT_IS_FALSE);
  	MOV(R0,SOB_FALSE);
  	JUMP(NOT_EXIT);
NOT_IS_FALSE:
 	MOV(R0,SOB_TRUE);
NOT_EXIT:
	POP(FP);
 	RETURN;


ZERO_PREDIC:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R1,FPARG(2));
  	CMP(IND(R1), T_INTEGER);
  	JUMP_EQ(ZERO_IS_INTEGER);
  	MOV(R0,SOB_FALSE);
  	JUMP(ZERO_EXIT);
ZERO_IS_INTEGER:
  	MOV(R2,INDD(R1,1));
  	CMP(R2,0);
  	JUMP_EQ(ZERO_IS_ZERO);
  	MOV(R0,SOB_FALSE);
  	JUMP(ZERO_EXIT);
ZERO_IS_ZERO:
  	MOV(R0,SOB_TRUE);
ZERO_EXIT:
	POP(FP);
 	RETURN; 

LESS_THAN_BINARY:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));

  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);

  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  MOV(R10,INDD(R1,1));
  MUL(R10, INDD(R2,2));

  MOV(R11,INDD(R2,1));
  MUL(R11, INDD(R1,2));

  CMP(R10,R11);
  JUMP_GE(LESS_THAN_LESS);
  MOV(R0,SOB_TRUE);
  JUMP(GREATER_THAN_CONTINUE);
LESS_THAN_LESS:
  MOV(R0,SOB_FALSE);
LESS_THAN_CONTINUE:
  POP(FP);
  RETURN;

GREATER_THAN_BINARY:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));

  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);

  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  MOV(R10,INDD(R1,1));
  MUL(R10, INDD(R2,2));

  MOV(R11,INDD(R2,1));
  MUL(R11, INDD(R1,2));

  CMP(R10,R11);
  JUMP_LE(GREATER_THAN_LESS);
  MOV(R0,SOB_TRUE);
  JUMP(GREATER_THAN_CONTINUE);
GREATER_THAN_LESS:
  MOV(R0,SOB_FALSE);
GREATER_THAN_CONTINUE:
  POP(FP);
  RETURN;


EQUAL_BINARY: // TODO : NEED TO BE EXTENDED TO SUPPORT FRACTION ETC...
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));

  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);

  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  CMP(INDD(R1,1),INDD(R2,1));
  JUMP_NE(EQUAL_NOT_EQUAL);
  CMP(INDD(R1,2),INDD(R2,2));
  JUMP_NE(EQUAL_NOT_EQUAL);
  MOV(R0,SOB_TRUE);
  JUMP(EQUAL_CONTINUE);
EQUAL_NOT_EQUAL:
  MOV(R0,SOB_FALSE);
EQUAL_CONTINUE:
  POP(FP);
  RETURN;


BINARY_MINUS: // TODO need to textend and support fraction
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));


  //  INFO;
  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);


  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  // MONE CALC
  MOV(R4,INDD(R1,1));
  MUL(R4,INDD(R2,2));


  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,1));

  SUB(R4,R5);

  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,2));

  PUSH(R5);
  PUSH(R4);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);

  POP(FP);
  RETURN;

BINARY_MUL: // TODO need to textend and support fraction
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));
  //  INFO;
  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);


  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  // MONE CALC
  MOV(R4,INDD(R1,1));
  MUL(R4,INDD(R2,1));

  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,2));


  PUSH(R5);
  PUSH(R4);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);


  POP(FP);
  RETURN;


BINARY_DIV: // TODO need to textend and support fraction
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));

  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);
  PUSH(R1);

  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  // MONE CALC
  MOV(R4,INDD(R1,1));
  MUL(R4,INDD(R2,2));

  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,1));


  PUSH(R5);
  PUSH(R4);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);


  POP(FP);
  RETURN;

ONARY_MINUS:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));

  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(INDD(R1,2));


  MOV(R5,INDD(R1,1));
  MUL(R5,-1);
  PUSH(R5);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  POP(FP);
  RETURN;

BINARY_PLUS: 
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  MOV(R2,FPARG(3));
  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R1,R0);

  PUSH(R1);


  MOV(R2,FPARG(3));
  PUSH(R2);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R2,R0);

  POP(R1);

  // MONE CALC
  MOV(R4,INDD(R1,1));
  MUL(R4,INDD(R2,2));


  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,1));

  ADD(R4,R5);

  MOV(R5,INDD(R1,2));
  MUL(R5,INDD(R2,2));

  PUSH(R5);
  PUSH(R4);
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  POP(FP);
  RETURN;


APPLY: 
  PUSH(FP);
  MOV(FP, SP);

  MOV(R0,FPARG(IMM(3))); // R0 = PARAM LIST

  MOV(R4, IMM(0));      // R4 = 0 => counter for the list size !

  APPLY_PARAMAS_LOOP:

  CMP(R0, SOB_NIL);
  JUMP_EQ(APPLY_END_PARAMS);
  // PUSH all the params to stack
  PUSH(INDD(R0, IMM(1)));  
  MOV(R0, INDD(R0, IMM(2))); 
  INCR(R4); 
  JUMP(APPLY_PARAMAS_LOOP);

  APPLY_END_PARAMS:

  MOV(R1, SP);
  SUB(R1, R4);

  //R1 = LAST PARAM ADDRESS

  MOV(R6,R1); // R6=R1= LAST PARAM ADDRESS

  MOV(R5, SP);
  DECR(R5);

  //R5 = FIRST PARAM ADDRESS

  APPLY_REVERSE_LOOP:
  CMP(R1, R5); 
  //ARE WE POINTING AT THE SAME ADDRRESS
  JUMP_GE(APPLY_END_REVERSE);


  // SWAPPING
  MOV(R3, STACK(R1)); 
  MOV(STACK(R1), STACK(R5));
  MOV(STACK(R5), R3);


  INCR(R1); // MOVING R1 POINTING
  DECR(R5); // MOVING R5 POINTING
  JUMP(APPLY_REVERSE_LOOP);


  APPLY_END_REVERSE:

  // R4 = number of args
  PUSH(R4); 
  MOV(R0, FPARG(IMM(2)));

  // pushing env
  PUSH(INDD(R0,1)); 
  MOV(R1,R6);
  DECR(R6);

  CMP(FPARG(-2),STACK(R6));
  JUMP_NE(APPLY_NOT_TAIL_POSITION);

  MOV(R1,FPARG(-1)); 
  PUSH(R1);

  MOV(R3,FP);
  SUB(R3,FPARG(-2));
  ADD(R4,3);
  MOV(R5,R4);
  MOV(R1,IMM(0));
  MOV(R6,FP);

  MOV(R10,FP);
  SUB(R10,FPARG(IMM(1))); 
  SUB(R10,4);
  MOV(R11,FPARG(-2));
  MOV(FP,R10);

  loop_label :
  CMP(R1,R4); 
  JUMP_EQ(loop_label_exit);
  MOV(R7,FP);
  ADD(R7,R1);
  MOV(R8,R6);
  ADD(R8,R1);
  MOV(STACK(R7),STACK(R8));
  INCR(R1);
  JUMP(loop_label );
  loop_label_exit :
  MOV(SP,FP);
  ADD(SP,R5);
  MOV(FP,R11);

  // JUMP TO THE CODE 
  JUMPA((INDD(R0 , IMM(2)))); 
  APPLY_NOT_TAIL_POSITION:
  CALLA(INDD(R0, 2));

  // DROP REST OF ARGS ON STACK
  MOV(R1,STARG(0)); 
  ADD(R1,2);
  DROP(IMM(R1));
  APPLY_EXIT:
  POP(FP);
  RETURN;


// Get a string pointer in R1, and return a link with a pointer to that string in R0
CREATE_SYM_LINK:
  PUSH(FP);
  MOV(FP,SP);
  
  PUSH(IMM(2)); //link size
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), R1); // mov the string to the link
  MOV(INDD(R0, 1), IMM(0)); // null pointer

  POP(FP);
  RETURN;


MINI_FRAC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(0)); // get num !!! not int

  PUSH(R1);
  CALL(ABS);
  DROP(1);
  MOV(R1,R0);
  PUSH(R1);

  MOV(R2,FPARG(1));
  PUSH(R2);
  CALL(ABS);
  DROP(1);
  MOV(R2,R0);

  POP(R1);



  MOV(R3,R1); //BIGGER
  MOV(R4,R2); //SMALLER
  CMP(R1,R2);
  JUMP_GE(MINI_FRAC_CONTINUE);
  MOV(R3,R2); //BIGGER
  MOV(R4,R1); //SMALLER
MINI_FRAC_CONTINUE:

  CMP(R4,0);
  JUMP_NE(MINI_FRAC_NOT_ZERO);

  PUSH(2);
  CALL(MALLOC);
  MOV(IND(R0),T_INTEGER);
  MOV(INDD(R0,1),IMM(0));
  DROP(1);
  POP(FP);
  RETURN;

MINI_FRAC_NOT_ZERO:
  PUSH(R4);
  PUSH(R3);
  CALL(GCD);
  DROP(2);


  MOV(R1,FPARG(0));
  MOV(R2,FPARG(1));
  MOV(R6,R1);
  MOV(R7,R2);
  DIV(R6,R0);
  DIV(R7,R0);




  PUSH(3);
  CALL(MALLOC);
  MOV(IND(R0),T_FRACTION);
  MOV(INDD(R0,1),R6);
  MOV(INDD(R0,2),R7);
  DROP(1);


  PUSH(R0);
  CALL(SIGN_NUMBER_CORRECTION);
  DROP(1);

  PUSH(R0);
  CALL(NUM_TO_INTEGER);
  DROP(1);


  POP(FP);
  RETURN; // returns sob

GCD:
  PUSH(FP);
  MOV(FP, SP);

  //CMP(FPARG(1),IMM(2));
  //JUMP_NE(L_ERROR_INCORRECT_NUMBER_OR_ARGS);
  MOV(R1,FPARG(0));
  //CMP(IND(R1),IMM(T_INTEGER));
  //JUMP_NE(L_ERROR_NOT_AN_INTEGER);
  MOV(R2,FPARG(1));
  //CMP(IND(R2),IMM(T_INTEGER));
  //JUMP_NE(L_ERROR_NOT_AN_INTEGER);


GCD_LOOP:
  MOV(R3,R1);
  REM(R3,R2);
  CMP(R3,IMM(0));
  JUMP_EQ(GCD_EXIT);
  MOV(R1,R2);
  MOV(R2,R3);
  JUMP(GCD_LOOP);

GCD_EXIT:
  MOV(R0,R2);

  POP(FP);
  RETURN;


NUM_TO_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(0));
  MOV(R0,R1);

  CMP(IND(R1),T_INTEGER);
  JUMP_NE(END_NUM_TO_FRACTION);

  PUSH(3);
  CALL(MALLOC);
  MOV(IND(R0),T_FRACTION);
  MOV(INDD(R0,1),INDD(R1,1));
  MOV(INDD(R0,2),IMM(1));
  DROP(1);

END_NUM_TO_FRACTION:
  POP(FP);
  RETURN;


NUM_TO_INTEGER: // IF BASE IS 1 -> RETURN INT ELSE FRAC
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1,FPARG(0));
  MOV(R0,R1);

  CMP(IND(R1),T_FRACTION);
  JUMP_NE(END_NUM_TO_INTEGER);

  CMP(INDD(R1,2),IMM(1));
  JUMP_NE(END_NUM_TO_INTEGER);

  PUSH(T_INTEGER);
  PUSH(INDD(R1,1));
  CALL(MAKE_SOB_INTEGER);
  DROP(2);

END_NUM_TO_INTEGER:
  POP(FP);
  RETURN;



MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(FPARG(1));
  PUSH(FPARG(0)); 
  CALL(MINI_FRAC);
  DROP(2);
  POP(FP);
  RETURN;


WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  DROP(1);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 2));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;


SIGN_NUMBER_CORRECTION:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1,FPARG(0));  


  MOV(R2,INDD(R1,1));
  MUL(R2,INDD(R1,2));
  CMP(R2,0);
  JUMP_GE(SIGN_HIUVI);
  
  PUSH(INDD(R1,1));
  CALL(ABS);
  DROP(1);
  MOV(R9,0);
  SUB(R9,R0);
  
  PUSH(R9);

  MOV(R1,FPARG(0)); 
  PUSH(INDD(R1,2));
  CALL(ABS);
  DROP(1);
  MOV(R7,R0);
  POP(R6);

  MOV(R0,FPARG(0));  
  MOV(INDD(R0,1),R6);
  MOV(INDD(R0,2),R7);

  JUMP(SIGN_NUMBER_CORRECTION_END);

SIGN_HIUVI:
  PUSH(INDD(R1,1));
  CALL(ABS);
  DROP(1);
  MOV(R9,0);
  PUSH(R0);

  MOV(R1,FPARG(0)); 
  PUSH(INDD(R1,2));
  CALL(ABS);
  DROP(1);
  MOV(R7,R0);
  POP(R6);

  MOV(R0,FPARG(0));  
  MOV(INDD(R0,1),R6);
  MOV(INDD(R0,2),R7);

SIGN_NUMBER_CORRECTION_END:

  POP(FP);
  RETURN;

DENOMINATOR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R0,INDD(R0,2))
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;


 NUMERATOR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(2));
  PUSH(R1);
  CALL(NUM_TO_FRACTION);
  DROP(1);
  MOV(R0,INDD(R0,1))
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN;

RATIONAL_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_FRACTION);
  JUMP_EQ(RATIONAL_PREDIC_TRUE);
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(RATIONAL_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(RATIONAL_PREDIC_EXIT);
RATIONAL_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
RATIONAL_PREDIC_EXIT:
  POP(FP);
  RETURN;

NUMBER_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_FRACTION);
  JUMP_EQ(NUMBER_PREDIC_TRUE);
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(NUMBER_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(NUMBER_PREDIC_EXIT);
NUMBER_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
NUMBER_PREDIC_EXIT:
  POP(FP);
  RETURN;

PROCEDURE_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_CLOSURE);
  JUMP_EQ(PROCEDURE_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(PROCEDURE_PREDIC_EXIT);
PROCEDURE_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
PROCEDURE_PREDIC_EXIT:
  POP(FP);
  RETURN;

SYMBOL_PREDIC:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  CMP(IND(R0), T_SYMBOL);
  JUMP_EQ(SYMBOL_PREDIC_TRUE);
  MOV(R0,SOB_FALSE);
  JUMP(SYMBOL_PREDIC_EXIT);
SYMBOL_PREDIC_TRUE:
  MOV(R0,SOB_TRUE);
SYMBOL_PREDIC_EXIT:
  POP(FP);
  RETURN;



SET_CAR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  MOV(R1, FPARG(3));
  MOV(INDD(R0,1), R1);
  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN;

SET_CDR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(2));
  MOV(R1, FPARG(3));
  MOV(INDD(R0,2), R1);
  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN


STRING_LENGTH:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R0,INDD(R1,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN


REMAINDER:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R2, FPARG(3));
  MOV(R3, INDD(R1,1));
  MOV(R4, INDD(R2,1));
  REM(R3,R4);
  PUSH(R3);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN


STRING_REF:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R2, FPARG(3));
  ADD(R1,2);
  MOV(R2, INDD(R2,1));
  MOV(R0,INDD(R1,R2));
  PUSH(R0);
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN

CHAR_TO_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R2, INDD(R1,1));
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN

INTEGER_TO_CHAR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1, FPARG(2));
  MOV(R2, INDD(R1,1));
  PUSH(R2);
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN

MAKE_STRING:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); // R1=SIZE
  MOV(R1,INDD(R1,1))
  ADD(R1,2);
  PUSH(R1);
  CALL(MALLOC);
  POP(R1);
  MOV(R10,R0);

  SUB(R1,2);

  MOV(INDD(R0,0), T_STRING);
  MOV(INDD(R0,1),R1);

  ADD(R0,1);

  MOV(R2,FPARG(1));

  MOV(R3,0);

  CMP(R2,1);
  JUMP_EQ(MAKE_STRING_ONE_ARG);
  MOV(R3,FPARG(3));
  MOV(R3,INDD(R3,1));

MAKE_STRING_ONE_ARG:

  CMP(R1,0);
  JUMP_EQ(MAKE_STRING_EXIT);
  INCR(R0);
  MOV(IND(R0), R3);
  DECR(R1);
  JUMP(MAKE_STRING_ONE_ARG);
MAKE_STRING_EXIT:
  MOV(R0,R10);
  POP(FP);
  RETURN;

MAKE_VECTOR:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); // R1=SIZE
  MOV(R1,INDD(R1,1))
  
  ADD(R1,2);
  PUSH(R1);
  CALL(MALLOC);
  DROP(1);

  MOV(R10,R0);

 

  MOV(R1, FPARG(2)); // R1=SIZE
  MOV(R1,INDD(R1,1))

  MOV(INDD(R0,0), T_VECTOR);
  MOV(INDD(R0,1),R1);
  ADD(R0,2);


  PUSH(R0);

  PUSH(0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  MOV(R3,R0);

  POP(R0);

  MOV(R1, FPARG(2)); // R1=SIZE
  MOV(R1,INDD(R1,1))

  MOV(R2,FPARG(1));
  CMP(R2,1);
  JUMP_EQ(MAKE_VECTOR_ONE_ARG);
  MOV(R3,FPARG(3));

MAKE_VECTOR_ONE_ARG:

  CMP(R1,0);
  JUMP_EQ(MAKE_VECTOR_EXIT);
  MOV(IND(R0), R3);
  DECR(R1);
  INCR(R0);
  JUMP(MAKE_VECTOR_ONE_ARG);

MAKE_VECTOR_EXIT:
  MOV(R0,R10);
  POP(FP);

  RETURN;


STRING_SET:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); // R1=pointer
  MOV(R2, FPARG(3)); // R2=index
  MOV(R3, FPARG(4)); // R3=char

  MOV(R2,INDD(R2,1));
  ADD(R2,2);
  MOV(INDD(R1,R2),INDD(R3,1));

  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN;


VECTOR:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(1)); // R1=SIZE
  ADD(R1,2);
  PUSH(R1);
  CALL(MALLOC);
  DROP(1);

  MOV(R1, FPARG(1)); // R1=SIZE
  MOV(R2, SP); // R2=first element
  SUB(R2,5);  
  MOV(INDD(R0,0), T_VECTOR);
  MOV(INDD(R0,1),R1);
  MOV(R10,R0);

  ADD(R0,2);

VECTOR_STARTLOOP:

  CMP(R1,0);
  JUMP_EQ(VECTOR_EXIT_END);
  MOV(IND(R0), STACK(R2));
  DECR(R2);
  INCR(R0);
  DECR(R1);
  JUMP(VECTOR_STARTLOOP);

VECTOR_EXIT_END:
  MOV(R0,R10);

  POP(FP);
  RETURN;



VECTOR_LENGTH:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); 
  MOV(R0, INDD(R1,1));
  PUSH(R0);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  POP(FP);
  RETURN

VECTOR_REF:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); // VEC 
  MOV(R2, FPARG(3)); // pos 
  MOV(R2,INDD(R2,1));
  ADD(R2,2);
  MOV(R0, INDD(R1,R2)); 
  POP(FP);
  RETURN;



VECTOR_SET:
  PUSH(FP);
  MOV(FP, SP);

  MOV(R1, FPARG(2)); // R1=pointer
  MOV(R2, FPARG(3)); // R2=index
  MOV(R3, FPARG(4)); // R3=char

  MOV(R2,INDD(R2,1));
  ADD(R2,2);
  MOV(INDD(R1,R2),R3);

  MOV(R0,SOB_VOID);
  POP(FP);
  RETURN

EQ:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  MOV(R0,FPARG(2));
  MOV(R1,FPARG(3));

  CMP(IND(R0),IND(R1));
  JUMP_NE(OBJ_NOT_EQ);
  
  CMP(IND(R0),T_FRACTION);
  JUMP_EQ(FRACTION_CHECK);
  
  CMP(IND(R0),T_INTEGER);
  JUMP_EQ(CMP_BY_VALUE);
  CMP(IND(R0),T_CHAR);
  JUMP_EQ(CMP_BY_VALUE);
  CMP(IND(R0),T_SYMBOL);
  JUMP_EQ(CMP_BY_VALUE);
  
  CMP(R0,R1);
  JUMP_NE(OBJ_NOT_EQ);
  JUMP(OBJ_ARE_EQ);
  
  CMP_BY_VALUE:
  CMP(INDD(R0,1),INDD(R1,1));
  JUMP_NE(OBJ_NOT_EQ);
  JUMP(OBJ_ARE_EQ);
  
  FRACTION_CHECK:
  CMP(INDD(R0,1),INDD(R1,1));
  MOV(R2,R0);
  CMP(INDD(R0,2),INDD(R1,2));
  AND(R0,R2);
  JUMP_NE(OBJ_NOT_EQ);
  JUMP(OBJ_ARE_EQ);
  
  OBJ_ARE_EQ:
  MOV(R0,SOB_TRUE);
  JUMP(EQ_EXIT);
  
  OBJ_NOT_EQ:
  MOV(R0,SOB_FALSE);
  EQ_EXIT:
  
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;


SYMBOL_TO_STRING:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0,FPARG(2));
  MOV(R0,INDD(R0,1)); //puts the symbol's string in R0
  POP(FP);
  RETURN;
  
STRING_TO_SYMBOL:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);

  MOV(R1,FPARG(2));
  MOV(R2,IND(1)); //start of string list
  CMP(R2,SOB_NIL);
  JUMP_EQ(STRING_TO_SYMBOL_LINK_NOT_FOUND);
  
  STRING_TO_SYMBOL_LOOP:
  PUSH(R1);
  PUSH(IND(R2));
  CALL(COMPARE_STRINGS);
  DROP(2);
  CMP(R0,SOB_TRUE);
  JUMP_EQ(STRING_TO_SYMBOL_LINK_FOUND);
  CMP(INDD(R2,1), SOB_NIL); //check if this is the last link in the list 
  JUMP_EQ(STRING_TO_SYMBOL_LINK_NOT_FOUND);
  MOV(R2,INDD(R2,1)); //get next link
  JUMP(STRING_TO_SYMBOL_LOOP);

  STRING_TO_SYMBOL_LINK_NOT_FOUND: //need to create the link
  PUSH(2);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),R1);
  MOV(INDD(R0,1),IND(1));
  MOV(IND(1),R0);
  MOV(R2,R0);

  STRING_TO_SYMBOL_LINK_FOUND: //R2 holds the string's link, create a SOB_SYMBOL
  PUSH(IND(R2));
  CALL(MAKE_SOB_SYMBOL);
  DROP(1);

  POP(R2);
  POP(R1);
  POP(FP);

  RETURN;
  
COMPARE_STRINGS:
  PUSH(FP);
  MOV(FP,SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);

  MOV(R0,SOB_TRUE);
  MOV(R1,FPARG(0));
  MOV(R2,FPARG(1));

  CMP(INDD(R1,1),INDD(R2,1)); //compare lengthes
  JUMP_NE(STRINGS_ARE_NOT_EQUAL);

  MOV(R3,INDD(R1,1)); //string's length
  MOV(R4,2); //points to string's first char

  COMPARE_STRINGS_LOOP:
  CMP(R3,IMM(0));
  JUMP_EQ(COMPARE_STRINGS_END);
  CMP(INDD(R1,R4),INDD(R2,R4)); //compare current char
  JUMP_NE(STRINGS_ARE_NOT_EQUAL);
  DECR(R3);
  INCR(R4);
  JUMP(COMPARE_STRINGS_LOOP);

  STRINGS_ARE_NOT_EQUAL:
  MOV(R0, SOB_FALSE);

  COMPARE_STRINGS_END:
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

