#ifndef BISON_EXPLANG_H
# define BISON_EXPLANG_H

#ifndef YYSTYPE
typedef union{
  /**
   * A type of those tokens which return a character.
   */
  char   ch;

  /**
   * A type of those tokens which return a character string.
   */
  char*  c_str;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	NAME	257
# define	OPEN_ARRAY_ELEM_LIST	258
# define	CLOSE_ARRAY_ELEM_LIST	259
# define	POWER_OP	260
# define	DEFINED_UNARY_FUNCTION	261
# define	DIGIT_STRING	262
# define	ENG_NOTATION	263
# define	SIGNIFICAND	264
# define	COMMENT	265
# define	EQ_OP	266
# define	NE_OP	267
# define	LT_OP	268
# define	LE_OP	269
# define	GT_OP	270
# define	GE_OP	271
# define	AND_OP	272
# define	OR_OP	273
# define	NOT_OP	274
# define	EQV_OP	275
# define	NEQV_OP	276
# define	EXIT	277
# define	IF	278
# define	THEN	279
# define	ELSE	280
# define	ENDIF	281
# define	TRUE	282
# define	FALSE	283
# define	EOF_MARKER	284
# define	SIGN	285


extern YYSTYPE nm_lval;

#endif /* not BISON_EXPLANG_H */
