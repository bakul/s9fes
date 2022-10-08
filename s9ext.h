/*
 * Scheme 9 from Empty Space, Refactored
 * By Nils M Holm, 2007-2019
 * In the public domain
 *
 * Interface for extension procedures.
 */

extern cell	Rts;
extern int	Sp;

#define parg(n)	car(vector(*GC_stack)[*GC_stkptr-(n)])
#define narg()	fixval(vector(*GC_stack)[*GC_stkptr])

#define BOL T_BOOLEAN  
#define CHR T_CHAR     
#define INP T_INPUT_PORT
#define INT T_INTEGER  
#define LST T_LIST     
#define OUP T_OUTPUT_PORT
#define PAI T_PAIR     
#define FUN T_FUNCTION 
#define REA T_REAL     
#define STR T_STRING   
#define SYM T_SYMBOL   
#define VEC T_VECTOR   
#define ___ T_ANY

void add_primitives(char *name, S9_PRIM *p);
void error(char *msg, cell expr);
cell integer_value(char *src, cell x);
