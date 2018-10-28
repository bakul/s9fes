/*
 * Scheme 9 from Empty Space, Reimagined
 * By Nils M Holm, 2007-2018
 * In the public domain
 * If your country does not have a public domain, the CC0 applies:
 * https://creativecommons.org/share-your-work/public-domain/cc0/
 */

#define RELEASE_DATE	"2018-10-28"
#define PATCHLEVEL	0

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#ifdef unix
 #include <signal.h>
 #include <setjmp.h>
 #define handle_sigquit()       signal(SIGQUIT, keyboard_quit)
 #define handle_sigint()        signal(SIGINT, keyboard_interrupt)
#endif
#ifdef plan9
 #define handle_sigquit()
 #define handle_sigint()        notify(keyboard_interrupt)
#endif

#define IMAGE_FILE	"s9.image"

#ifndef IMAGE_DIR
 #define IMAGE_DIR	"."
#endif

#ifndef LIBRARY_PATH
 #ifdef unix
  #define LIBRARY_PATH \
		"."				\
		":lib"				\
		":ext/unix"			\
		":ext/csv"			\
		":ext/curses"			\
		":contrib"			\
		":~/.s9fes"			\
		":/usr/local/share/s9fes"
 #endif
 #ifdef plan9
  #define LIBRARY_PATH \
		"."		\
		":lib"		\
		":ext/csv"	\
		":ext/plan9"	\
		":contrib"	\
		":~/lib/s9fes"
 #endif
#endif

/*
 * Configurable options
 */

#define TOKEN_LENGTH	1024
#define MAX_PORTS	32
#define MAX_IO_DEPTH	65536
#define MAX_REF_TRACE	5
#define MAX_EXPAND	10000
#define CHUNK_SIZE	1024

/*
 * Globals
 */

cell	Tmp = NIL;

cell	Env = NIL,
	Envp = NIL;

cell	Glob = NIL,
	Hash = NIL;

cell	Macros = NIL;

cell	Trace[MAX_REF_TRACE];
int	Tp = 0;

cell	Emitbuf;

cell	Prog = NIL;
int	Here = 0;

cell	Cts = NIL,
	Rts = NIL;

volatile int Running = 1;
volatile int Intr = 0;

int	Stats = 0;

cell	Acc = NIL;

cell	E0 = NIL,
	Ep = NIL;

int	Ip = 0,
	Sp = -1,
	Fp = -1;

int	Sz = CHUNK_SIZE;

jmp_buf	Restart;
jmp_buf	Error_tag;
cell	Error_handler;

cell	Argv = NIL;

char	Srcfile[TOKEN_LENGTH+1] = { 0 };
int	Line_no = 1;
int	Level = 0;
int	Opening_line = 0;
int	Displaying = 0;

volatile int	Expand_level = 0;

char	S9magic[17];

int	O_quiet = 0;

cell	*Image_vars[] = { &Glob, &Hash, &Macros, NULL };

cell	*GC_roots[] = {
		&Prog, &Env, &Cts, &Emitbuf, &Glob, &Hash,
		&Macros, &Rts, &Acc, &Ep, &E0, &Argv, &Tmp,
	NULL };

/*
 * Pre-defined symbols
 */

/* Internals */

cell	I_arg, I_closure, I_ref, I_a, I_e;

/* Special Ops */

cell    S_apply, S_arguments, S_begin, S_define,
	S_define_syntax, S_epsilon, S_error_tag, S_error_value,
	S_extensions, S_host_system, S_if, S_ifstar,
	S_image_file, S_lambda, S_letrec, S_library_path,
	S_loading, S_quasiquote, S_quote, S_release_date,
	S_set_b, S_starstar, S_unquote, S_unquote_splicing;

/* Procedures */

cell	P_abs, P_append, P_assq, P_assv, P_bit_op, P_boolean_p,
	P_caaaar, P_caaadr, P_caaar, P_caadar, P_caaddr,
	P_caadr, P_caar, P_cadaar, P_cadadr, P_cadar, P_caddar,
	P_cadddr, P_caddr, P_cadr, P_call_cc,
	P_call_with_current_continuation, P_car, P_catch,
	P_catch_tag_p, P_cdaaar, P_cdaadr, P_cdaar, P_cdadar,
	P_cdaddr, P_cdadr, P_cdar, P_cddaar, P_cddadr, P_cddar,
	P_cdddar, P_cddddr, P_cdddr, P_cddr, P_cdr, P_ceiling,
	P_char_alphabetic_p, P_char_ci_equal_p,
	P_char_ci_grtr_p, P_char_ci_gteq_p, P_char_ci_less_p,
	P_char_ci_lteq_p, P_char_downcase, P_char_equal_p,
	P_char_grtr_p, P_char_gteq_p, P_char_less_p,
	P_char_lower_case_p, P_char_lteq_p, P_char_numeric_p,
	P_char_p, P_char_to_integer, P_char_upcase,
	P_char_upper_case_p, P_char_whitespace_p,
	P_close_input_port, P_close_output_port, P_command_line,
	P_cons, P_current_error_port, P_current_input_port,
	P_current_output_port, P_delete_file, P_display,
	P_divide, P_dump_image, P_environment_variable,
	P_eof_object_p, P_eq_p, P_equal, P_eqv_p, P_error,
	P_eval, P_even_p, P_exact_p, P_exact_to_inexact,
	P_exponent, P_expt, P_file_exists_p, P_floor, P_gensym,
	P_grtr, P_gteq, P_inexact_p, P_inexact_to_exact,
	P_input_port_p, P_integer_p, P_integer_p,
	P_integer_to_char, P_length, P_less, P_list, P_list_ref,
	P_list_tail, P_list_to_string, P_list_to_string,
	P_list_to_vector, P_list_to_vector, P_load, P_lteq,
	P_macro_expand, P_macro_expand_1, P_make_string,
	P_make_vector, P_mantissa, P_max, P_memq, P_memv, P_min,
	P_minus, P_negative_p, P_not, P_null_p, P_odd_p,
	P_number_p, P_open_append_file, P_open_input_file,
	P_open_output_file, P_output_port_p, P_pair_p,
	P_peek_char, P_plus, P_positive_p, P_procedure_p,
	P_quit, P_quotient, P_read, P_read_char, P_real_p,
	P_remainder, P_reverse, P_reverse_b, P_s9_bytecode,
	P_set_car_b, P_set_cdr_b, P_set_input_port_b,
	P_set_output_port_b, P_stats, P_string_append,
	P_string_ci_equal, P_string_ci_grtr, P_string_ci_gteq,
	P_string_ci_less, P_string_ci_lteq, P_string_copy,
	P_string_equal, P_string_fill_b, P_string_grtr,
	P_string_gteq, P_string_length, P_string_less,
	P_string_lteq, P_string_p, P_string_ref, P_string_set_b,
	P_string_to_list, P_string_to_symbol, P_substring,
	P_symbol_p, P_symbol_p, P_symbol_to_string, P_symbols,
	P_system_command, P_throw, P_times, P_truncate,
	P_vector, P_vector_append, P_vector_copy,
	P_vector_fill_b, P_vector_length, P_vector_p,
	P_vector_ref, P_vector_set_b, P_vector_to_list, P_write,
	P_write_char, P_zero_p;

/*
 * Abstract machine opcodes
 */

enum	{ OP_APPLIS, OP_APPLY, OP_ARG, OP_COPY_ARG, OP_CLOSURE,
	  OP_COPY_REF, OP_DEF_MACRO, OP_ENTER, OP_ENTER_COLL,
	  OP_HALT, OP_JMP, OP_JMP_FALSE, OP_JMP_TRUE, OP_MAKE_ENV,
	  OP_PROP_ENV, OP_POP, OP_PUSH, OP_PUSH_VAL, OP_REF,
	  OP_RETURN, OP_SET_ARG, OP_SET_REF, OP_TAIL_APPLIS,
	  OP_TAIL_APPLY,

	  OP_ABS, OP_APPEND, OP_ARGV, OP_ASSQ, OP_ASSV,
	  OP_BIT_OP, OP_BOOLEAN_P, OP_CAAAAR, OP_CAAADR,
	  OP_CAAAR, OP_CAADAR, OP_CAADDR, OP_CAADR, OP_CAAR,
	  OP_CADAAR, OP_CADADR, OP_CADAR, OP_CADDAR, OP_CADDDR,
	  OP_CADDR, OP_CADR, OP_CALL_CC, OP_CAR, OP_CATCH,
	  OP_CATCH_TAG_P, OP_CDAAAR, OP_CDAADR, OP_CDAAR,
	  OP_CDADAR, OP_CDADDR, OP_CDADR, OP_CDAR, OP_CDDAAR,
	  OP_CDDADR, OP_CDDAR, OP_CDDDAR, OP_CDDDDR, OP_CDDDR,
	  OP_CDDR, OP_CDR, OP_CEILING, OP_CHAR_ALPHABETIC_P,
	  OP_CHAR_CI_EQUAL_P, OP_CHAR_CI_GRTR_P,
	  OP_CHAR_CI_GTEQ_P, OP_CHAR_CI_LESS_P,
	  OP_CHAR_CI_LTEQ_P, OP_CHAR_DOWNCASE, OP_CHAR_EQUAL_P,
	  OP_CHAR_GRTR_P, OP_CHAR_GTEQ_P, OP_CHAR_LESS_P,
	  OP_CHAR_LOWER_CASE_P, OP_CHAR_LTEQ_P,
	  OP_CHAR_NUMERIC_P, OP_CHAR_P, OP_CHAR_TO_INTEGER,
	  OP_CHAR_UPCASE, OP_CHAR_UPPER_CASE_P,
	  OP_CHAR_WHITESPACE_P, OP_CLOSE_INPUT_PORT,
	  OP_CLOSE_OUTPUT_PORT, OP_COMMAND_LINE, OP_CONS,
	  OP_CURRENT_ERROR_PORT, OP_CURRENT_INPUT_PORT,
	  OP_CURRENT_OUTPUT_PORT, OP_DELETE_FILE, OP_DISPLAY,
	  OP_DIVIDE, OP_DUMP_IMAGE, OP_ENVIRONMENT_VARIABLE,
	  OP_EOF_OBJECT_P, OP_EQUAL, OP_EQV_P, OP_EQ_P,
	  OP_ERROR, OP_ERROR2, OP_EVAL, OP_EVEN_P, OP_EXACT_P,
	  OP_EXACT_TO_INEXACT, OP_EXPONENT, OP_EXPT,
	  OP_FILE_EXISTS_P, OP_FIX_EXACTNESS, OP_FLOOR,
	  OP_GENSYM, OP_GRTR, OP_GTEQ, OP_INEXACT_P,
	  OP_INEXACT_TO_EXACT, OP_INPUT_PORT_P, OP_INTEGER_P,
	  OP_INTEGER_TO_CHAR, OP_LENGTH, OP_LESS, OP_LIST,
	  OP_LIST_REF, OP_LIST_TAIL, OP_LIST_TO_STRING,
	  OP_LIST_TO_VECTOR, OP_LOAD, OP_LTEQ, OP_MACRO_EXPAND,
	  OP_MACRO_EXPAND_1, OP_MAKE_STRING, OP_MAKE_VECTOR,
	  OP_MANTISSA, OP_MAX, OP_MEMQ, OP_MEMV, OP_MIN,
	  OP_MINUS, OP_NEGATE, OP_NEGATIVE_P, OP_NOT, OP_NULL_P,
	  OP_ODD_P, OP_OPEN_APPEND_FILE, OP_OPEN_INPUT_FILE,
	  OP_OPEN_OUTPUT_FILE, OP_OUTPUT_PORT_P, OP_PAIR_P,
	  OP_PEEK_CHAR, OP_PLUS, OP_POSITIVE_P, OP_PROCEDURE_P,
	  OP_QUIT, OP_QUOTE, OP_QUOTIENT, OP_READ, OP_READ_CHAR,
	  OP_REAL_P, OP_REMAINDER, OP_REVERSE, OP_REVERSE_B,
	  OP_S9_BYTECODE, OP_SET_CAR_B, OP_SET_CDR_B,
	  OP_SET_INPUT_PORT_B, OP_SET_OUTPUT_PORT_B, OP_STATS,
	  OP_STRING_APPEND, OP_STRING_COPY, OP_STRING_EQUAL_P,
	  OP_STRING_FILL_B, OP_STRING_GRTR_P, OP_STRING_GTEQ_P,
	  OP_STRING_LENGTH, OP_STRING_LESS_P, OP_STRING_LTEQ_P,
	  OP_STRING_P, OP_STRING_REF, OP_STRING_SET_B,
	  OP_STRING_SI_EQUAL_P, OP_STRING_SI_GRTR_P,
	  OP_STRING_SI_GTEQ_P, OP_STRING_SI_LESS_P,
	  OP_STRING_SI_LTEQ_P, OP_STRING_TO_LIST,
	  OP_STRING_TO_SYMBOL, OP_SUBSTRING, OP_SYMBOLS,
	  OP_SYMBOL_P, OP_SYMBOL_TO_STRING, OP_SYSTEM_COMMAND,
	  OP_THROW, OP_TIMES, OP_TRUNCATE, OP_UNQUOTE,
	  OP_UNQUOTE_SPLICING, OP_VECTOR, OP_VECTOR_APPEND,
	  OP_VECTOR_COPY, OP_VECTOR_FILL_B, OP_VECTOR_LENGTH,
	  OP_VECTOR_P, OP_VECTOR_REF, OP_VECTOR_SET_B,
	  OP_VECTOR_TO_LIST, OP_WRITE, OP_WRITE_CHAR, OP_ZERO_P
	  };

/*
 * Types
 */

#define RPAREN	(USER_SPECIALS-1)
#define RBRACK	(USER_SPECIALS-2)
#define DOT	(USER_SPECIALS-3)

#define T_FIXNUM	(USER_SPECIALS-100)
#define T_CATCH_TAG	(USER_SPECIALS-101)

/*
 * Extension setup, add your own ones here
 */

void sys_init(void);
void curs_init(void);
void csv_init(void);

#ifndef EXTENSIONS
 #define EXTENSIONS
#endif

/*
 * Error reporting and handling
 */

void	prints(char *s);
void	print_form(cell x);

char	*ntoa(char *b, cell x, int w);

cell	getbind(cell x);

void rerror(char *s, cell x) {
	int	i, j, o;
	char	buf[100];

	Error_handler = getbind(S_error_tag);
	if (Error_handler != NIL) longjmp(Error_tag, 1);
	s9_abort();
	o = set_output_port(O_quiet? 2: 1);
	prints("*** error: ");
	prints(s);
	if (x != UNDEFINED) {
		prints(": ");
		set_printer_limit(100);
		print_form(x);
		set_printer_limit(0);
	}
	nl();
	if (Srcfile[0] != 0) {
		prints("*** file \"");
		prints(Srcfile);
		prints("\", line ");
		prints(ntoa(buf, Line_no, 0));
		nl();
	}
	prints("*** trace:");
	i = Tp;
	for (j=0; j<MAX_REF_TRACE; j++) {
		if (i >= MAX_REF_TRACE) i = 0;
		if (Trace[i] != NIL) {
			prints(" ");
			print_form(Trace[i]);
		}
		i++;
	}
	nl();
	set_output_port(o);
}

void error(char *s, cell x) {
	rerror(s, x);
	longjmp(Restart, 1);
}

void expect(char *who, char *what, cell got) {
	char	b[100];

	sprintf(b, "%s: expected %s, got", who, what);
	error(b, got);
}

/*
 * Type implementations
 */

cell mkfix(int v) {
	cell	n;

	n = new_atom(v, NIL);
	return new_atom(T_FIXNUM, n);
}

#define fix_p(n) \
        (!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && T_FIXNUM == car(n))

#define fixval(x) cadr(x)

cell closure(cell i, cell e) {
	cell	c;

	c = cons(Prog, NIL);
	c = cons(e, c);
	c = cons(i, c);
	return new_atom(T_FUNCTION, c);
}

#define closure_ip(c)	cadr(c)
#define closure_env(c)	caddr(c)
#define closure_prog(c)	cadddr(c)

cell catch(void) {
	cell	n;

	         n = cons(Prog, NIL);
	Tmp = n; n = cons(Ep, n);
	Tmp = n; n = cons(mkfix(Fp), n);
	Tmp = n; n = cons(mkfix(Sp), n);
	Tmp = n; n = cons(mkfix(Ip+2), n);
	Tmp = NIL;
	return new_atom(T_CATCH_TAG, n);
}

#define catch_tag_p(n) \
        (!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && T_CATCH_TAG == car(n))

int throw(cell ct, cell v) {
	ct = cdr(ct);
	Ip = fixval(car(ct)); ct = cdr(ct);
	Sp = fixval(car(ct)); ct = cdr(ct);
	Fp = fixval(car(ct)); ct = cdr(ct);
	Ep = car(ct);         ct = cdr(ct);
	Prog = car(ct);
	Acc = v;
	return Ip;
}

cell subvector(cell v, int k0, int k1);

cell capture_cont(void) {
	cell	n, *v;
	int	i;

	n = cons(subvector(Rts, 0, Sz), NIL);
	v = vector(car(n));
	for (i = Sp+1; i < Sz; i++) v[i] = UNDEFINED;
	Tmp = n; n = cons(mkfix(Sz), n);
	Tmp = n; n = cons(Prog, n);
	Tmp = n; n = cons(Ep, n);
	Tmp = n; n = cons(mkfix(Fp), n);
	Tmp = n; n = cons(mkfix(Sp), n);
	Tmp = n; n = cons(mkfix(Ip+2), n);
	Tmp = NIL;
	return new_atom(T_CONTINUATION, n);
}

int call_cont(cell c, cell v) {
	int	nsp;

	c = cdr(c);
	Ip  = fixval(car(c)); c = cdr(c);
	nsp = fixval(car(c)); c = cdr(c);
	Fp  = fixval(car(c)); c = cdr(c);
	Ep  = car(c);         c = cdr(c);
	Prog = car(c);        c = cdr(c);
	Sz  = fixval(car(c)); c = cdr(c);
	Rts = subvector(car(c), 0, Sz);
	Sp = nsp;
	Acc = v;
	return Ip;
}

#define list_p(x) (pair_p(x) || NIL == (x))

unsigned hash(char *s) {
	unsigned int	h = 0;

	while (*s) h = ((h<<5)+h) ^ *s++;
	return h;
}

int hash_size(int n) {
	if (n < 47) return 47;
	if (n < 97) return 97;
	if (n < 199) return 199;
	if (n < 499) return 499;
	if (n < 997) return 997;
	if (n < 9973) return 9973;
	if (n < 19997) return 19997;
	return 39989;
}

void rehash(void) {
	unsigned int	i;
	cell		*v, n, p, new;
	unsigned int	h, k;

	if (NIL == Hash)
		k = hash_size(length(Env));
	else
		k = hash_size(fixval(vector(Hash)[0]));
	Hash = new_vec(T_VECTOR, (k+1) * sizeof(cell));
	v = vector(Hash);
	for (i=1; i<=k; i++) v[i] = NIL;
	i = 0;
	for (p = Env; p != NIL; p = cdr(p)) {
		h = hash(symbol_name(car(p)));
		n = cons(car(p), mkfix(i));
		n = cons(n, vector(Hash)[h%k+1]);
		vector(Hash)[h%k+1] = n;
		i++;
	}
	new = mkfix(i);
	vector(Hash)[0] = new;
}

void addhash(cell x) {
	cell		n, new;
	unsigned int	h, i, k;

	if (NIL == Hash) {
		rehash();
		return;
	}
	i = fixval(vector(Hash)[0]);
	k = vector_len(Hash)-1;
	if (i > k) {
		rehash();
		return;
	}
	h = hash(symbol_name(x));
	n = cons(x, mkfix(i));
	n = cons(n, vector(Hash)[h%k+1]);
	vector(Hash)[h%k+1] = n;
	new = mkfix(i+1);
	vector(Hash)[0] = new;
}

int lookup(cell x) {
	unsigned int	h, k;
	cell		n;

	k = vector_len(Hash)-1;
	h = hash(symbol_name(x));
	for (n = vector(Hash)[h%k+1]; n != NIL; n = cdr(n))
		if (x == caar(n))
			return fixval(cdar(n));
	return FALSE;
}

/*
 * Low-level utility functions
 */

int strcmp_ci(char *s1, char *s2) {
	int	c1, c2;

	while (1) {
		c1 = tolower((int) *s1++);
		c2 = tolower((int) *s2++);
		if ('\0' == c1 || '\0' == c2 || c1 != c2)
			break;
	}
	return c1 < c2? -1: c1 > c2? 1: 0;
}

char *ntoa(char *b, cell x, int w) {
	char	buf[40];
	int	i = 0, neg = 0;
	char	*p = &buf[sizeof(buf)-1];

	if (x < 0) {
		x = -x;
		neg = 1;
	}
	*p = 0;
	while (x || 0 == i) {
		i++;
		if (i >= sizeof(buf)-1)
			fatal("ntoa: number too big");
		p--;
		*p = x % 10 + '0';
		x = x / 10;
	}
	while (i < (w-neg) && i < sizeof(buf)-1) {
		i++;
		p--;
		*p = '0';
	}
	if (neg) {
		if (i >= sizeof(buf)-1)
			fatal("ntoa: number too big");
		p--;
		*p = '-';
	}
	strcpy(b, p);
	return b;
}

cell reverse(cell n) {
	cell	m;

	m = NIL;
	while (n != NIL) {
		if (atom_p(n)) error("reverse: improper list", n);
                m = cons(car(n), m);
		n = cdr(n);
        }
        return m;
}

cell nreverse(cell n) {
	cell	h, m;

	m = NIL;
	while (n != NIL) {
		if (atom_p(n)) error("reverse!: improper list", n);
		h = cdr(n);
		cdr(n) = m;
		m = n;
		n = h;
	}
	return m;
}

cell conc(cell a, cell b) {
	cell	n;

	a = reverse(a);
	save(a);
	n = b;
	while (a != NIL) {
		n = cons(car(a), n);
		a = cdr(a);
	}
	unsave(1);
	return n;
}

cell nconc(cell a, cell b) {
	cell	n;

	n = a;
	if (NIL == a) return b;
	while (cdr(a) != NIL)
		a = cdr(a);
	cdr(a) = b;
	return n;
}

int memq(cell x, cell a) {
	if (!symbol_p(x)) return FALSE;
	for (; a != NIL; a = cdr(a))
		if (car(a) == x) return a;
	return FALSE;
}

int assq(cell x, cell a) {
	if (!symbol_p(x)) return FALSE;
	for (; a != NIL; a = cdr(a))
		if (caar(a) == x) return car(a);
	return FALSE;
}

int posq(cell x, cell a) {
	int	n;

	if (!symbol_p(x)) return FALSE;
	n = 0;
	for (; a != NIL; a = cdr(a)) {
		if (car(a) == x) return n;
		n++;
	}
	return FALSE;
}

int hashq(cell x, cell e) {
	if (!symbol_p(x)) return FALSE;
	if (e == Env) return lookup(x);
	return posq(x, e);
}

cell set_union(cell a, cell b) {
	cell	n;

	a = reverse(a);
	save(a);
	save(n = b);
	while (pair_p(a)) {
		if (memq(car(a), b) == FALSE) {
			n = cons(car(a), n);
			car(Stack) = n;
		}
		a = cdr(a);
	}
	if (a != NIL && memq(a, b) == FALSE)
		n = cons(a, n);
	unsave(2);
	return n;
}

cell flatargs(cell a) {
	cell	n;

	save(n = NIL);
	while (pair_p(a)) {
		n = cons(car(a), n);
		car(Stack) = n;
		a = cdr(a);
	}
	if (a != NIL) n = cons(a, n);
	unsave(1);
	return nreverse(n);
}

cell dotted_p(cell x) {
	while (pair_p(x)) x = cdr(x);
	return x != NIL;
}

cell carof(cell a) {
	cell	n;

	save(n = NIL);
	while (a != NIL) {
		n = cons(caar(a), n);
		car(Stack) = n;
		a = cdr(a);
	}
	unsave(1);
	return nreverse(n);
}

cell zip(cell a, cell b) {
	cell	n, p;

	save(n = NIL);
	while (a != NIL && b != NIL) {
		p = cons(car(a), car(b));
		n = cons(p, n);
		car(Stack) = n;
		a = cdr(a);
		b = cdr(b);
	}
	unsave(1);
	return nreverse(n);
}

cell lastpair(cell x) {
	if (NIL == x) return NIL;
	while (cdr(x) != NIL)
		x = cdr(x);
	return x;
}

cell exists_p(char *s) {
	FILE	*f;

	f = fopen(s, "r");
	if (f != NULL) fclose(f);
	return NULL == f? FALSE: TRUE;
}

cell subvector(cell x, int k0, int k1) {
	cell	n, *vx, *vn;
	int	i, j;

	n = make_vector(k1-k0);
	vx = vector(x);
	vn = vector(n);
	j = 0;
	for (i=k0; i<k1; i++) {
		vn[j] = vx[i];
		j++;
	}
	return n;
}

cell list_to_vector(cell m, char *msg, int flags) {
	cell	n, vec;
	int	k;
	cell	*p;

	k = 0;
	for (n = m; n != NIL; n = cdr(n)) {
		if (atom_p(n)) error(msg, m);
		k++;
	}
	if (0 == k) return make_vector(0);
	vec = new_vec(T_VECTOR, k*sizeof(cell));
	Tag[vec] |= flags;
	p = vector(vec);
	for (n = m; n != NIL; n = cdr(n)) {
		*p = car(n);
		p++;
	}
	return vec;
}

cell list_to_string(cell x) {
	cell	n;
	int	k = length(x);
	char	*s;

	n = make_string("", k);
	s = string(n);
	while (x != NIL) {
		if (atom_p(x))
			error("list->string: improper list", x);
		if (!char_p(car(x))) {
			error("list->string: expected list of char,"
				" got list containing",
				car(x));
		}
		*s++ = char_value(car(x));
		x = cdr(x);
	}
	*s = 0;
	return n;
}

cell string_to_list(cell x) {
	char	*s;
	cell	n, a, new;
	int	k, i;

	k = string_len(x);
	n = NIL;
	a = NIL;
	for (i=0; i<k-1; i++) {
		s = string(x);
		if (NIL == n) {
			n = a = cons(make_char(s[i]), NIL);
			save(n);
		}
		else {
			new = cons(make_char(s[i]), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL) unsave(1);
	return n;
}

cell vector_to_list(cell x) {
	cell	n, a, new;
	int	k, i;

	k = vector_len(x);
	n = NIL;
	a = NIL;
	for (i=0; i<k; i++) {
		if (NIL == n) {
			n = a = cons(vector(x)[i], NIL);
			save(n);
		}
		else {
			new = cons(vector(x)[i], NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL) unsave(1);
	return n;
}

/*
 * Interpreter-side access to bindings.
 * Using deep binding here. At program run time
 * shallow binding will be used exclusively.
 */

void envbind(cell v, cell a) {
	cell	n;

	n = cons(a, NIL);
	n = cons(v, n);
	Glob = cons(n, Glob);
}

void setbind(cell v, cell a) {
	cell	b;

	b = assq(v, Glob);
	if (b != FALSE) cadr(b) = a;
}

cell getbind(cell v) {
	cell	b;

	b = assq(v, Glob);
	if (NIL == b) error("internal variable undefined", v);
	return cadr(b);
}

/*
 * System initialization
 */

void clear_trace(void) {
	int	i;

	for (i=0; i<MAX_REF_TRACE; i++) Trace[i] = NIL;
}

cell make_library_path(void) {
	char	path[TOKEN_LENGTH+1], *s;
	int	i, j;
	cell	n, new;

	save(n = NIL);
	s = getenv("S9FES_LIBRARY_PATH");
	if (NULL == s) s = LIBRARY_PATH;
	i = j = 0;
	for (;;) {
		if ('\0' == s[i] || ':' == s[i]) {
			path[j] = 0;
			new = make_string(path, j);
			j = 0;
			n = cons(new, n);
			car(Stack) = n;
			if ('\0' == s[i]) break;
			i++;
		}
		if (j >= TOKEN_LENGTH)
			fatal("library path element too long");
		path[j++] = s[i];
		i++;
	}
	return nreverse(unsave(1));
}

void add_primitives(char *name, S9_PRIM *p) {
	cell	x, v, n, b;
	int	i;

	if (name) {
		n = symbol_ref(name);
		x = getbind(S_extensions);
		x = nconc(x, cons(n, NIL));
		setbind(S_extensions, x);
	}
	save(b = NIL);
	for (i=0; p && p[i].name; i++) {
		v = symbol_ref(p[i].name);
		n = cons(make_primitive(&p[i]), NIL);
		n = cons(v, n);
		b = cons(n, b);
		car(Stack) = b;
	}
	b = nreverse(b);
	unsave(1);
	Glob = nconc(Glob, b);
}

cell eval(cell x);

void init_extensions(void) {
	cell	n, p;
	#define	LEN 100
	char	b[LEN+2], *s;

	for (n = getbind(S_extensions); n != NIL; n = cdr(n)) {
		if (symbol_len(car(n)) > LEN/2)
			error("extension init name too long", car(n));
		s = symbol_name(car(n));
		sprintf(b, "%s:%s", s, s);
		p = symbol_ref(b);
		p = assq(p, Glob);
		if (FALSE == p) continue;
		p = cons(cadr(p), NIL);
		eval(p);
	}
	p = symbol_ref("s9:s9");
	p = assq(p, Glob);
	if (FALSE == p) return;
	p = cons(cadr(p), NIL);
	eval(p);
}

void init_rts(void) {
	Rts = NIL;
	Rts = make_vector(CHUNK_SIZE);
	Sz = CHUNK_SIZE;
	Sp = -1;
	Fp = -1;
}

void init(void) {
	s9_init(GC_roots, &Rts, &Sp);
	init_rts();
	image_vars(Image_vars);
	exponent_chars("eEdDfFlLsS");
	memset(S9magic, 0, sizeof(S9magic));
	if (strlen(RELEASE_DATE) == 10)
		sprintf(S9magic, "S9:%s:%c", RELEASE_DATE, PATCHLEVEL+'0');
	else
		strcpy(S9magic, "S9:BAD-VERSION");
	clear_trace();
	I_a = symbol_ref("a");
	I_e = symbol_ref("e");
	I_arg = symbol_ref("%arg");
	I_closure = symbol_ref("%closure");
	I_ref = symbol_ref("%ref");
	S_apply = symbol_ref("apply");
	S_arguments = symbol_ref("*arguments*");
	S_begin = symbol_ref("begin");
	S_define = symbol_ref("define");
	S_define_syntax = symbol_ref("define-syntax");
	S_epsilon = symbol_ref("*epsilon*");
	S_error_tag = symbol_ref("*error-tag*");
	S_error_value = symbol_ref("*error-value*");
	S_extensions = symbol_ref("*extensions*");
	S_host_system = symbol_ref("*host-system*");
	S_if = symbol_ref("if");
	S_ifstar = symbol_ref("if*");
	S_image_file = symbol_ref("*image-file*");
	S_lambda = symbol_ref("lambda");
	S_letrec = symbol_ref("letrec");
	S_library_path = symbol_ref("*library-path*");
	S_loading = symbol_ref("*loading*");
	S_quasiquote = symbol_ref("quasiquote");
	S_quote = symbol_ref("quote");
	S_release_date = symbol_ref("*release-date*");
	S_set_b = symbol_ref("set!");
	S_starstar = symbol_ref("**");
	S_unquote = symbol_ref("unquote");
	S_unquote_splicing = symbol_ref("unquote-splicing");
	P_abs = symbol_ref("abs");
	P_append = symbol_ref("append");
	P_assq = symbol_ref("assq");
	P_assv = symbol_ref("assv");
	P_bit_op = symbol_ref("bit-op");
	P_boolean_p = symbol_ref("boolean?");
	P_caaaar = symbol_ref("caaaar");
	P_caaadr = symbol_ref("caaadr");
	P_caaar = symbol_ref("caaar");
	P_caadar = symbol_ref("caadar");
	P_caaddr = symbol_ref("caaddr");
	P_caadr = symbol_ref("caadr");
	P_caar = symbol_ref("caar");
	P_cadaar = symbol_ref("cadaar");
	P_cadadr = symbol_ref("cadadr");
	P_cadar = symbol_ref("cadar");
	P_caddar = symbol_ref("caddar");
	P_cadddr = symbol_ref("cadddr");
	P_caddr = symbol_ref("caddr");
	P_cadr = symbol_ref("cadr");
	P_call_cc = symbol_ref("call/cc");
	P_call_with_current_continuation =
		symbol_ref("call-with-current-continuation");
	P_car = symbol_ref("car");
	P_catch = symbol_ref("catch");
	P_catch_tag_p = symbol_ref("catch-tag?");
	P_cdaaar = symbol_ref("cdaaar");
	P_cdaadr = symbol_ref("cdaadr");
	P_cdaar = symbol_ref("cdaar");
	P_cdadar = symbol_ref("cdadar");
	P_cdaddr = symbol_ref("cdaddr");
	P_cdadr = symbol_ref("cdadr");
	P_cdar = symbol_ref("cdar");
	P_cddaar = symbol_ref("cddaar");
	P_cddadr = symbol_ref("cddadr");
	P_cddar = symbol_ref("cddar");
	P_cdddar = symbol_ref("cdddar");
	P_cddddr = symbol_ref("cddddr");
	P_cdddr = symbol_ref("cdddr");
	P_cddr = symbol_ref("cddr");
	P_cdr = symbol_ref("cdr");
	P_ceiling = symbol_ref("ceiling");
	P_char_alphabetic_p = symbol_ref("char-alphabetic?");
	P_char_ci_equal_p = symbol_ref("char-ci=?");
	P_char_ci_grtr_p = symbol_ref("char-ci>?");
	P_char_ci_gteq_p = symbol_ref("char-ci>=?");
	P_char_ci_less_p = symbol_ref("char-ci<?");
	P_char_ci_lteq_p = symbol_ref("char-ci<=?");
	P_char_alphabetic_p = symbol_ref("char-alphabetic?");
	P_char_downcase = symbol_ref("char-downcase");
	P_char_equal_p = symbol_ref("char=?");
	P_char_grtr_p = symbol_ref("char>?");
	P_char_gteq_p = symbol_ref("char>=?");
	P_char_less_p = symbol_ref("char<?");
	P_char_lower_case_p = symbol_ref("char-lower-case?");
	P_char_lteq_p = symbol_ref("char<=?");
	P_char_numeric_p = symbol_ref("char-numeric?");
	P_char_p = symbol_ref("char?");
	P_char_to_integer = symbol_ref("char->integer");
	P_char_upcase = symbol_ref("char-upcase");
	P_char_upper_case_p = symbol_ref("char-upper-case?");
	P_char_whitespace_p = symbol_ref("char-whitespace?");
	P_close_input_port = symbol_ref("close-input-port");
	P_close_output_port = symbol_ref("close-output-port");
	P_command_line = symbol_ref("command-line");
	P_cons = symbol_ref("cons");
	P_current_error_port = symbol_ref("current-error-port");
	P_current_input_port = symbol_ref("current-input-port");
	P_current_output_port = symbol_ref("current-output-port");
	P_delete_file = symbol_ref("delete-file");
	P_display = symbol_ref("display");
	P_divide = symbol_ref("/");
	P_dump_image = symbol_ref("dump-image");
	P_environment_variable = symbol_ref("environment-variable");
	P_eof_object_p = symbol_ref("eof-object?");
	P_eq_p = symbol_ref("eq?");
	P_equal = symbol_ref("=");
	P_eqv_p = symbol_ref("eqv?");
	P_error = symbol_ref("error");
	P_eval = symbol_ref("eval");
	P_even_p = symbol_ref("even?");
	P_exact_p = symbol_ref("exact?");
	P_exact_to_inexact = symbol_ref("exact->inexact");
	P_exponent = symbol_ref("exponent");
	P_expt = symbol_ref("expt");
	P_file_exists_p = symbol_ref("file-exists?");
	P_floor = symbol_ref("floor");
	P_gensym = symbol_ref("gensym");
	P_grtr = symbol_ref(">");
	P_gteq = symbol_ref(">=");
	P_inexact_p = symbol_ref("inexact?");
	P_inexact_to_exact = symbol_ref("inexact->exact");
	P_input_port_p = symbol_ref("input-port?");
	P_integer_p = symbol_ref("integer?");
	P_integer_p = symbol_ref("integer?");
	P_integer_to_char = symbol_ref("integer->char");
	P_length = symbol_ref("length");
	P_less = symbol_ref("<");
	P_list = symbol_ref("list");
	P_list_ref = symbol_ref("list-ref");
	P_list_tail = symbol_ref("list-tail");
	P_list_to_string = symbol_ref("list->string");
	P_list_to_string = symbol_ref("list->string");
	P_list_to_vector = symbol_ref("list->vector");
	P_list_to_vector = symbol_ref("list->vector");
	P_load = symbol_ref("load");
	P_lteq = symbol_ref("<=");
	P_macro_expand = symbol_ref("macro-expand");
	P_macro_expand_1 = symbol_ref("macro-expand-1");
	P_make_string = symbol_ref("make-string");
	P_make_vector = symbol_ref("make-vector");
	P_mantissa = symbol_ref("mantissa");
	P_max = symbol_ref("max");
	P_memq = symbol_ref("memq");
	P_memv = symbol_ref("memv");
	P_min = symbol_ref("min");
	P_minus = symbol_ref("-");
	P_negative_p = symbol_ref("negative?");
	P_not = symbol_ref("not");
	P_null_p = symbol_ref("null?");
	P_number_p = symbol_ref("number?");
	P_odd_p = symbol_ref("odd?");
	P_open_append_file = symbol_ref("open-append-file");
	P_open_input_file = symbol_ref("open-input-file");
	P_open_output_file = symbol_ref("open-output-file");
	P_output_port_p = symbol_ref("output-port?");
	P_pair_p = symbol_ref("pair?");
	P_peek_char = symbol_ref("peek-char");
	P_plus = symbol_ref("+");
	P_positive_p = symbol_ref("positive?");
	P_procedure_p = symbol_ref("procedure?");
	P_quit = symbol_ref("quit");
	P_quotient = symbol_ref("quotient");
	P_read = symbol_ref("read");
	P_read_char = symbol_ref("read-char");
	P_real_p = symbol_ref("real?");
	P_remainder = symbol_ref("remainder");
	P_reverse = symbol_ref("reverse");
	P_reverse_b = symbol_ref("reverse!");
	P_s9_bytecode = symbol_ref("s9:bytecode");
	P_set_car_b = symbol_ref("set-car!");
	P_set_cdr_b = symbol_ref("set-cdr!");
	P_set_input_port_b = symbol_ref("set-input-port!");
	P_set_output_port_b = symbol_ref("set-output-port!");
	P_stats = symbol_ref("stats");
	P_string_copy = symbol_ref("string-copy");
	P_string_append = symbol_ref("string-append");
	P_string_ci_equal = symbol_ref("string-ci=?");
	P_string_ci_grtr = symbol_ref("string-ci>?");
	P_string_ci_gteq = symbol_ref("string-ci>=?");
	P_string_ci_less = symbol_ref("string-ci<?");
	P_string_ci_lteq = symbol_ref("string-ci<=?");
	P_string_equal = symbol_ref("string=?");
	P_string_fill_b = symbol_ref("string-fill!");
	P_string_grtr = symbol_ref("string>?");
	P_string_gteq = symbol_ref("string>=?");
	P_string_length = symbol_ref("string-length");
	P_string_less = symbol_ref("string<?");
	P_string_lteq = symbol_ref("string<=?");
	P_string_p = symbol_ref("string?");
	P_string_ref = symbol_ref("string-ref");
	P_string_set_b = symbol_ref("string-set!");
	P_string_to_list = symbol_ref("string->list");
	P_string_to_symbol = symbol_ref("string->symbol");
	P_substring = symbol_ref("substring");
	P_symbol_p = symbol_ref("symbol?");
	P_symbol_to_string = symbol_ref("symbol->string");
	P_symbols = symbol_ref("symbols");
	P_system_command = symbol_ref("system-command");
	P_throw = symbol_ref("throw");
	P_times = symbol_ref("*");
	P_truncate = symbol_ref("truncate");
	P_vector = symbol_ref("vector");
	P_vector_append = symbol_ref("vector-append");
	P_vector_copy = symbol_ref("vector-copy");
	P_vector_fill_b = symbol_ref("vector-fill!");
	P_vector_length = symbol_ref("vector-length");
	P_vector_p = symbol_ref("vector?");
	P_vector_ref = symbol_ref("vector-ref");
	P_vector_set_b = symbol_ref("vector-set!");
	P_vector_to_list = symbol_ref("vector->list");
	P_write = symbol_ref("write");
	P_write_char = symbol_ref("write-char");
	P_zero_p = symbol_ref("zero?");
	envbind(S_arguments, NIL);
	envbind(S_epsilon, Epsilon);
	envbind(S_error_tag, NIL);
	envbind(S_error_value, NIL);
	envbind(S_extensions, NIL);
	envbind(S_image_file, FALSE);
	envbind(S_library_path, make_library_path());
	envbind(S_loading, FALSE);
	envbind(S_release_date, make_string(RELEASE_DATE,
					strlen(RELEASE_DATE)));
	envbind(S_starstar, NIL);
#ifdef unix
	envbind(S_host_system, symbol_ref("unix"));
#else
 #ifdef plan9
	envbind(S_host_system, symbol_ref("plan9"));
 #else
	envbind(S_host_system, symbol_ref("unknown"));
 #endif
#endif
	EXTENSIONS
}

void loadfile(char *s);

void load_initial_image(char *image) {
	char	*path, pbuf[TOKEN_LENGTH+1];
	char	*s, *imgdir;

	if (setjmp(Restart) != 0)
		fatal("could not load S9 image or library");
	imgdir = getenv("S9FES_IMAGE_DIR");
	if (NULL == imgdir) imgdir = IMAGE_DIR;
	if ('.' == *image || '/' == *image) {
		path = image;
	}
	else {
		if (strlen(image) + strlen(imgdir) > TOKEN_LENGTH)
			error("image path too long", UNDEFINED);
		sprintf(pbuf, "%s/%s", imgdir, image);
		path = pbuf;
	}
	if (strcmp(image, "-") == 0) {
		loadfile("s9.scm");
	}
	else if (exists_p(path) != FALSE) {
		s = load_image(path, S9magic);
		if (s != NULL) {
			error(s, make_string(path, strlen(path)));
			fatal("aborting");
		}
		setbind(S_image_file,
			make_string(path, strlen(path)));
	}
	else if (exists_p(IMAGE_FILE) != FALSE) {
		s = load_image(IMAGE_FILE, S9magic);
		if (s != NULL) {
			error(s, make_string(path, strlen(path)));
			fatal("aborting");
		}
		setbind(S_image_file,
			make_string(IMAGE_FILE, strlen(IMAGE_FILE)));
	}
	else {
		error("cannot open image file",
			make_string(path, strlen(path)));
	}
}

/*
 * Reader
 */

cell read_form(int flags);

cell read_list(int flags, int delim) {
	cell	n, m, a;
	int	c;
	cell	new;
	char	badpair[] = "malformed pair";
	char	msg[80];

	if (!Level)
		Opening_line = Line_no;
	if (++Level > MAX_IO_DEPTH) {
		error("reader: too many nested lists or vectors", UNDEFINED);
		return NIL;
	}
	m = cons3(NIL, NIL, flags);
	save(m);
	a = NIL;
	c = 0;
	while (1) {
		if (Intr) {
			unsave(1);
			return NIL;
		}
		n = read_form(flags);
		if (END_OF_FILE == n)  {
			sprintf(msg, "missing ')', started in line %d",
					Opening_line);
			error(msg, UNDEFINED);
		}
		if (DOT == n) {
			if (c < 1) {
				error(badpair, UNDEFINED);
				continue;
			}
			n = read_form(flags);
			cdr(a) = n;
			if (n == delim || read_form(flags) != delim) {
				error(badpair, UNDEFINED);
				continue;
			}
			unsave(1);
			Level--;
			return m;
		}
		if (RPAREN == n || RBRACK == n) {
			if (n != delim)
				error(RPAREN == n?
				  "list starting with `[' ended with `)'":
				  "list starting with `(' ended with `]'",
				  UNDEFINED);
			break;
		}
		if (NIL == a)
			a = m;
		else
			a = cdr(a);
		car(a) = n;
		new = cons3(NIL, NIL, flags);
		cdr(a) = new;
		c++;
	}
	Level--;
	if (a != NIL) cdr(a) = NIL;
	unsave(1);
	return c? m: NIL;
}

cell quote(cell n, cell quotation) {
	cell	q;

	q = cons(n, NIL);
	return cons(quotation, q);
}

cell read_character(void) {
	char	buf[10], msg[50];
	int	i, c = 0; /*LINT*/

	for (i=0; i<sizeof(buf)-1; i++) {
		if (Intr) return NIL;
		c = readc();
		if (i > 0 && !isalpha(c))
			break;
		buf[i] = c;
	}
	rejectc(c);
	buf[i] = 0;
	if (0 == i)
		c = ' ';
	else if (1 == i)
		c = buf[0];
	else if (!strcmp_ci(buf, "space"))
		c = ' ';
	else if (!strcmp_ci(buf, "newline"))
		c = '\n';
	else {
		sprintf(msg, "unknown character: #\\%s", buf);
		error(msg, UNDEFINED);
		c = 0;
	}
	return make_char(c);
}

cell read_string(void) {
	char	s[TOKEN_LENGTH+1];
	cell	n;
	int	c, i, q;
	int	inv;

	i = 0;
	q = 0;
	c = readc();
	inv = 0;
	while (q || c != '"') {
		if (Intr) return NIL;
		if ('\n' == c)
			Line_no++;
		if (EOF == c)
			error("missing '\"' in string literal", UNDEFINED);
		if (i >= TOKEN_LENGTH-2) {
			error("string literal too long", UNDEFINED);
			i--;
		}
		if (q && c != '"' && c != '\\' && c != 'n') {
			s[i++] = '\\';
			inv = 1;
		}
		s[i] = q && 'n' == c? '\n': c;
		q = !q && '\\' == c;
		if (!q) i++;
		c = readc();
	}
	s[i] = 0;
	n = make_string(s, i);
	Tag[n] |= S9_CONST_TAG;
	if (inv) error("invalid escape sequence in string", n);
	return n;
}

#define separator(c) \
	(' '  == (c) || '\t' == (c) || '\n' == (c) || \
	 '\r' == (c) || '('  == (c) || ')'  == (c) || \
	 ';'  == (c) || '\'' == (c) || '`'  == (c) || \
	 ','  == (c) || '"'  == (c) || '['  == (c) || \
	 ']'  == (c) || EOF  == (c))

#define SYM_CHARS	"!@$%^&*-/_+=~.?<>:"

#define is_symbolic(c) \
	(isalpha(c) || \
	 isdigit(c) || \
	 strchr(SYM_CHARS, (c)))

void funny_char(char *msg, int c) {
	char	buf[128];

	if (isprint(c)) error(msg, make_char(c));
	sprintf(buf, "%s, code", msg);
	error(buf, make_integer(c));
}

#define readc_ci()     tolower(readc())

cell read_symbol_or_number(int c) {
	char	s[TOKEN_LENGTH];
	int	i, funny = 0;

	i = 0;
	while (!separator(c)) {
		if (!is_symbolic(c))
			funny = c;
		if (i >= TOKEN_LENGTH-2) {
			error("symbol too long", UNDEFINED);
			i--;
		}
		s[i] = c;
		i++;
		c = readc_ci();
	}
	s[i] = 0;
	rejectc(c);
	if (funny) funny_char("funny character in symbol", funny);
	if (string_numeric_p(s))
		return string_to_number(s);
	if (!strcmp(s, "define-macro"))
		return S_define_syntax;
	return symbol_ref(s);
}

cell read_vector(void) {
	cell	n;

	n = read_list(0, RPAREN);
	save(n);
	n = list_to_vector(n, "invalid vector syntax", S9_CONST_TAG);
	unsave(1);
	return n;
}

cell meta_command(void) {
	int	c, cmd, i;
	cell	n, cmdsym;
	char	s[128];

	cmd = readc_ci();
	c = readc();
	while (' ' == c)
		c = readc();
	i = 0;
	while (c != '\n' && c != EOF) {
		if (i < sizeof(s) - 2)
			s[i++] = c;
		c = readc();
	}
	rejectc(c);
	s[i] = 0;
	n = make_string(s, strlen(s));
	n = 0 == i? NIL: cons(n, NIL);
	save(n);
	switch (cmd) {
	case 'a':	cmdsym = symbol_ref("apropos"); break;
	case 'h':	cmdsym = symbol_ref("help"); break;
	case 'l':	cmdsym = symbol_ref("load-from-library"); break;
	case 'q':	cmdsym = symbol_ref("quit"); break;
	default: 	prints(",a = apropos"); nl();
			prints(",h = help"); nl();
			prints(",l = load-from-library"); nl();
			prints(",q = quit"); nl();
			return UNSPECIFIC;
	}
	unsave(1);
	return cons(cmdsym, n);
}

int block_comment(void) {
	int	n, c, state = 0;

	for (n=1; n; ) {
		c = readc_ci();
		switch (c) {
		case EOF:
			error("missing |#", UNDEFINED);
			return 0;
		case '|':
			switch (state) {
			case 1:		n++; state = 0; break;
			default:	state = -1; break;
			}
			break;
		case '#':
			switch (state) {
			case -1:	n--; state = 0; break;
			default:	state = 1; break;
			}
			break;
		case '\n':
			Line_no++;
			state = 0;
			break;
		default:
			state = 0;
			break;
		}
	}
	return readc_ci();
}

int closing_paren(void) {
	int c = readc_ci();

	rejectc(c);
	return ')' == c || ']' == c;
}

cell bignum_read(char *pre, int radix) {
	char	digits[] = "0123456789abcdef";
	char	buf[100];
	cell	base, num;
	int	c, s, p, nd;

	base = make_integer(radix);
	save(base);
	num = Zero;
	save(num);
	c = readc_ci();
	s = 0;
	if ('-' == c) {
		s = 1;
		c = readc_ci();
	}
	else if ('+' == c) {
		c = readc_ci();
	}
	nd = 0;
	while (!separator(c)) {
		p = 0;
		while (digits[p] && digits[p] != c)
			p++;
		if (p >= radix) {
			sprintf(buf, "invalid digit in %s number", pre);
			unsave(2);
			funny_char(buf, c);
		}
		num = bignum_multiply(num, base);
		car(Stack) = num;
		num = bignum_add(num, make_integer(p));
		car(Stack) = num;
		nd++;
		c = readc_ci();
	}
	unsave(2);
	if (!nd) {
		sprintf(buf, "digits expected after %s", pre);
		error(buf, UNDEFINED);
	}
	rejectc(c);
	return s? bignum_negate(num): num;
}


cell read_real_number(int inexact) {
	cell	n, m;
	int	flags;
	char	buf[50];

	n = read_form(0);
	if (integer_p(n)) {
		if (!inexact) return n;
		flags = bignum_negative_p(n)? REAL_NEGATIVE: 0;
		m = bignum_abs(n);
		return Make_real(flags, 0, cdr(m));
	}
	else if (real_p(n)) {
		if (inexact) return n;
		m = real_to_bignum(n);
		if (UNDEFINED == m)
			error("#e: no exact representation for", n);
		return m;
	}
	sprintf(buf, "number expected after #%c, got",
		inexact? 'i': 'e');
	error(buf, n);
	return UNDEFINED;
}

cell unreadable(void) {
	int	c, i;
	char	buf[TOKEN_LENGTH];
	int	d;

	strcpy(buf, "#<");
	i = 2;
	while (1) {
		c = readc_ci();
		if ('>' == c || '\n' == c) {
			if ('\n' == c) Line_no++;
			break;
		}
		if (i < TOKEN_LENGTH-2)
			buf[i++] = c;
	}
	buf[i++] = '>';
	buf[i] = 0;
	d = Displaying;
	Displaying = 1;
	error("unreadable object", make_string(buf, i));
	Displaying = d;
	return UNDEFINED;
}

cell read_form(int flags) {
	char	buf[50];
	int	c, c2;

	c = readc_ci();
	while (1) {
		while (' ' == c || '\t' == c || '\n' == c || '\r' == c) {
			if (c == '\n') Line_no++;
			if (Intr) return NIL;
			c = readc_ci();
		}
		if ('#' == c) {
			c = readc_ci();
			if ('!' == c) {
				/* skip rest of line */
			}
			else if ('|' == c) {
				c = block_comment();
				continue;
			}
			else {
				rejectc(c);
				c = '#';
				break;
			}
		}
		else if (c != ';') {
			break;
		}
		while (0 == Intr && c != '\n' && c != EOF)
			c = readc_ci();
		if (Intr) return NIL;
	}
	if (EOF == c) {
		return END_OF_FILE;
	}
	if (Intr) {
		return NIL;
	}
	if ('(' == c) {
		return read_list(flags, RPAREN);
	}
	else if ('[' == c) {
		return read_list(flags, RBRACK);
	}
	else if ('\'' == c || '`' == c) {
		cell	n;

		if (closing_paren())
			error("missing form after \"'\" or \"`\"",
				UNDEFINED);
		Level++;
		n = quote(read_form(S9_CONST_TAG),
			'`' == c? S_quasiquote: S_quote);
		Level--;
		return n;
	}
	else if (',' == c) {
		if (closing_paren())
			error("missing form after \",\"",
				UNDEFINED);
		c = readc_ci();
		if ('@' == c) {
			return quote(read_form(0), S_unquote_splicing);
		}
		else {
			rejectc(c);
			if (!Level)
				return meta_command();
			return quote(read_form(0), S_unquote);
		}
	}
	else if ('#' == c) {
		c = readc_ci();
		switch (c) {
		case 'f':	return FALSE;
		case 't':	return TRUE;
		case '\\':	return read_character();
		case '(':	return read_vector();
		case 'b':	return bignum_read("#b", 2);
		case 'd':	return bignum_read("#d", 10);
		case 'o':	return bignum_read("#o", 8);
		case 'x':	return bignum_read("#x", 16);
		case 'e':	return read_real_number(0);
		case 'i':	return read_real_number(1);
		case '<':	return unreadable();
		default:	sprintf(buf, "unknown # syntax: #%c", c);
				error(buf, UNDEFINED);
				return UNDEFINED;
		}
	}
	else if ('"' == c) {
		return read_string();
	}
	else if (')' == c) {
		if (!Level) error("unexpected ')'", UNDEFINED);
		return RPAREN;
	}
	else if (']' == c) {
		if (!Level) error("unexpected ']'", UNDEFINED);
		return RBRACK;
	}
	else if ('.' == c) {
		c2 = readc_ci();
		rejectc(c2);
		if (separator(c2)) {
			if (!Level) error("unexpected '.'", UNDEFINED);
			return DOT;
		}
		return read_symbol_or_number(c);
	}
	else if (is_symbolic(c)) {
		return read_symbol_or_number(c);
	}
	else {
		funny_char("funny input character", c);
		return UNDEFINED;
	}
}

cell xread(void) {
	Level = 0;
	return read_form(0);
}

cell xsread(char *s) {
	cell	n;

	open_input_string(s);
	n = read_form(0);
	close_input_string();
	return n;
}

/*
 * Printer
 */

int print_integer(cell n) {
	if (!integer_p(n)) return 0;
	print_bignum(n);
	return 1;
}

int print_realnum(cell n) {
	if (!real_p(n)) return 0;
	print_real(n);
	return 1;
}

void print_form(cell x);

int print_quoted(cell n) {
	if (	car(n) == S_quote &&
		cdr(n) != NIL &&
		cddr(n) == NIL
	) {
		prints("'");
		print_form(cadr(n));
		return 1;
	}
	return 0;
}

int print_procedure(cell n) {
	if (function_p(n)) {
		prints("#<procedure>");
		return 1;
	}
	return 0;
}

int print_catch_tag(cell n) {
	if (catch_tag_p(n)) {
		prints("#<catch tag>");
		return 1;
	}
	return 0;
}

int print_continuation(cell n) {
	if (continuation_p(n)) {
		prints("#<continuation>");
		return 1;
	}
	return 0;
}

int print_char(cell n) {
	char	b[2];
	int	c;

	if (!char_p(n))
		return 0;
	if (!Displaying)
		prints("#\\");
	c = cadr(n);
	b[1] = 0;
	if (!Displaying && ' ' == c)
		prints("space");
	else if (!Displaying && '\n' == c)
		prints("newline");
	else {
		b[0] = c;
		prints(b);
	}
	return 1;
}

int print_string(cell n) {
	char	b[2];
	int	k;
	char	*s;

	if (!string_p(n))
		return 0;
	if (!Displaying)
		prints("\"");
	s = string(n);
	k = string_len(n)-1;
	b[1] = 0;
	while (k) {
		b[0] = *s++;
		if (Displaying) {
			prints(b);
		}
		else if ('"' == b[0] || '\\' == b[0]) {
			prints("\\");
			prints(b);
		}
		else if ('\n' == b[0]) {
			prints("\\n");
		}
		else {
			prints(b);
		}
		k--;
	}
	if (!Displaying)
		prints("\"");
	return 1;
}

int print_symbol(cell n) {
	char	*s;

	if (!symbol_p(n)) return 0;
	s = symbol_name(n);
	prints(s);
	return 1;
}

int print_primitive(cell n) {
	S9_PRIM	*p;

	if (!primitive_p(n)) return 0;
	prints("#<primitive ");
	p = &Primitives[cadr(n)];
	prints(p->name);
	prints(">");
	return 1;
}

int print_vector(cell n) {
	cell	*p;
	int	k;

	if (!vector_p(n)) return 0;
	prints("#(");
	p = vector(n);
	k = vector_len(n);
	while (k--) {
		print_form(*p++);
		if (k) prints(" ");
	}
	prints(")");
	return 1;
}

int print_port(cell n) {
	char	buf[100];

	if (!input_port_p(n) && !output_port_p(n))
		return 0;
	sprintf(buf, "#<%s-port %d>",
		input_port_p(n)? "input": "output",
		(int) port_no(n));
	prints(buf);
	return 1;
}

int print_fixnum(cell n) {
	char	buf[100];

	if (!fix_p(n)) return 0;
	sprintf(buf, "#<fix %d>", (int) fixval(n));
	prints(buf);
	return 1;
}

void print_special(cell n) {
	char	buf[100];

	sprintf(buf, "#<unknown special value %d>", (int) n);
	prints(buf);
}

void x_print_form(cell n, int depth) {
	if (depth > MAX_IO_DEPTH) {
		error("printer: too many nested lists or vectors", UNDEFINED);
		return;
	}
	if (NIL == n) {
		prints("()");
	}
	else if (eof_p(n)) {
		prints("#<eof>");
	}
	else if (FALSE == n) {
		prints("#f");
	}
	else if (TRUE == n) {
		prints("#t");
	}
	else if (undefined_p(n)) {
		prints("#<undefined>");
	}
	else if (unspecific_p(n)) {
		prints("#<unspecific>");
	}
	else if (special_p(n)) {
		print_special(n);
	}
	else {
		if (print_char(n)) return;
		if (print_procedure(n)) return;
		if (print_catch_tag(n)) return;
		if (print_continuation(n)) return;
		if (print_realnum(n)) return;
		if (print_integer(n)) return;
		if (print_primitive(n)) return;
		if (print_quoted(n)) return;
		if (print_string(n)) return;
		if (print_symbol(n)) return;
		if (print_vector(n)) return;
		if (print_port(n)) return;
		if (print_fixnum(n)) return;
		prints("(");
		while (n != NIL) {
			if (printer_limit())
				return;
			x_print_form(car(n), depth+1);
			n = cdr(n);
			if (n != NIL && atom_p(n)) {
				prints(" . ");
				x_print_form(n, depth+1);
				n = NIL;
			}
			if (n != NIL) prints(" ");
		}
		prints(")");
	}
}

void print_form(cell n) {
	x_print_form(n, 0);
}

/*
 * Syntax checker
 */

void ckargs(cell x, char *who, int min, int max) {
	int	k;
	char	buf[100];

	k = length(x)-1;
	if (k < min || (k > max && max >= 0)) {
		sprintf(buf, "%s: wrong number of arguments", who);
		error(buf, x);
	}
}

int syncheck(cell x, int top);

int ckseq(cell x, int top) {
	for (; pair_p(x); x = cdr(x))
		syncheck(car(x), top);
	return 0;
}

int ckapply(cell x) {
	ckargs(x, "apply", 2, -1);
	return 0;
}

int ckbegin(cell x, int top) {
	return ckseq(cdr(x), top);
}

int ckdefine(cell x, int top) {
	ckargs(x, "define", 2, 2);
	if (!symbol_p(cadr(x)))
		error("define: expected symbol", cadr(x));
	if (0 == top)
		error("define: must be at top level", x);
	return ckseq(cddr(x), 0);
}

int ckif(cell x) {
	ckargs(x, "if", 2, 3);
	return ckseq(cdr(x), 0);
}

int ckifstar(cell x) {
	ckargs(x, "if*", 2, 2);
	return ckseq(cdr(x), 0);
}

int symlistp(cell x) {
	cell	p;

	for (p = x; pair_p(p); p = cdr(p)) {
		if (!symbol_p(car(p)))
			return 0;
	}
	return symbol_p(p) || NIL == p;
}

int uniqlist(cell x) {
	if (NIL == x) return 1;
	while (cdr(x) != NIL) {
		if (memq(car(x), cdr(x)) != FALSE)
			return 0;
		x = cdr(x);
	}
	return 1;
}

int cklambda(cell x) {
	ckargs(x, "lambda", 2, -1);
	if (	!symbol_p(cadr(x)) &&
		cadr(x) != NIL &&
		!symlistp(cadr(x)))
	{
		error("lambda: invalid formals", cadr(x));
	}
	if (!uniqlist(flatargs(cadr(x))))
		error("lambda: duplicate formal", cadr(x));
	return ckseq(cddr(x), 0);
}

int ckmacro(cell x, int top) {
	ckargs(x, "define-syntax", 2, 2);
	if (!symbol_p(cadr(x)))
		error("define-syntax: expected symbol", cadr(x));
	if (0 == top)
		error("define-syntax: must be at top level", x);
	return ckseq(cddr(x), 0);
}

int ckquote(cell x) {
	ckargs(x, "quote", 1, 1);
	return 0;
}

int cksetb(cell x) {
	ckargs(x, "set!", 2, 2);
	if (!symbol_p(cadr(x)))
		error("set!: expected symbol", cadr(x));
	return ckseq(cddr(x), 0);
}

int syncheck(cell x, int top) {
	cell	p;

	if (atom_p(x)) return 0;
	for (p = x; pair_p(p); p = cdr(p))
		;
	if (p != NIL)
		error("improper list in program", x);
	if (car(x) == S_apply) return ckapply(x);
	if (car(x) == S_begin) return ckbegin(x, top);
	if (car(x) == S_define) return ckdefine(x, top);
	if (car(x) == S_define_syntax) return ckmacro(x, top);
	if (car(x) == S_if) return ckif(x);
	if (car(x) == S_ifstar) return ckifstar(x);
	if (car(x) == S_lambda) return cklambda(x);
	if (car(x) == S_quote) return ckquote(x);
	if (car(x) == S_set_b) return cksetb(x);
	return ckseq(x, 0);
}

/*
 * Bytecode compiler
 */

/* Closure conversion */

int subrp(cell x);

cell free_vars(cell x, cell e) {
	cell	n, u, a;
	int	lam;

	lam = 0;
	if (memq(x, e) != FALSE) {
		return NIL;
	}
	else if (symbol_p(x)) {
		return cons(x, NIL);
	}
	else if (!pair_p(x)) {
		return NIL;
	}
	else if (car(x) == S_quote) {
		return NIL;
	}
	else if (car(x) == S_apply ||
		 car(x) == S_begin ||
		 car(x) == S_if ||
		 car(x) == S_ifstar ||
		 car(x) == S_set_b
	) {
		x = cdr(x);
	}
	else if (car(x) == S_define ||
		 car(x) == S_define_syntax
	) {
		x = cddr(x);
	}
	else if (subrp(car(x))) {
		x = cdr(x);
	}
	else if (car(x) == S_lambda) {
		save(e);
		a = flatargs(cadr(x));
		save(a);
		n = set_union(a, e);
		save(n);
		e = n;
		x = cddr(x);
		lam = 1;
	}
	save(u = NIL);
	while (pair_p(x)) {
		n = free_vars(car(x), e);
		save(n);
		u = set_union(u, n);
		unsave(1);
		car(Stack) = u;
		x = cdr(x);
	}
	n = unsave(1);
	if (lam) e = unsave(3);
	return n;
}

cell cconv(cell x, cell e, cell a);

cell mapconv(cell x, cell e, cell a) {
	cell	n, new;

	save(n = NIL);
	while (pair_p(x)) {
		new = cconv(car(x), e, a);
		n = cons(new, n);
		car(Stack) = n;
		x = cdr(x);
	}
	return nreverse(unsave(1));
}

cell initmap(cell fv, cell e, cell a) {
	cell	m, n, p;
	int	i, j;

	save(m = NIL);
	i = 0;
	while (fv != NIL) {
		p = cons(car(fv), NIL);
		save(p);
		n = mkfix(i);
		p = cons(n, p);
		car(Stack) = p;
		if ((j = posq(car(fv), a)) != FALSE) {
			n = mkfix(j);
			p = cons(n, p);
			unsave(1);
			p = cons(I_a, p);
		}
		else if ((j = hashq(car(fv), e)) != FALSE) {
			n = mkfix(j);
			p = cons(n, p);
			unsave(1);
			p = cons(I_e, p);
		}
		else {
			error("undefined symbol", car(fv));
		}
		m = cons(p, m);
		car(Stack) = m;
		i++;
		fv = cdr(fv);
	}
	return nreverse(unsave(1));
}

void newvar(cell x) {
	cell	n;

	if (memq(x, Env) != FALSE) return;
	if (NIL == Envp) Envp = lastpair(Env);
	n = cons(x, NIL);
	cdr(Envp) = n;
	Envp = n;
	addhash(x);
}

void newvars(cell x) {
	while (x != NIL) {
		newvar(car(x));
		x = cdr(x);
	}
}

cell lamconv(cell x, cell e, cell a) {
	cell	cl, fv, args, m;

	fv = free_vars(x, NIL);
	save(fv);
	newvars(fv);
	args = flatargs(cadr(x));
	save(args);
	m = initmap(fv, e, a);
	save(m);
	cl = mapconv(cddr(x), fv, args);
	cl = cons(m, cl);
	cl = cons(cadr(x), cl);
	cl = cons(I_closure, cl);
	unsave(3);
	return cl;
}

int contains(cell a, cell x) {
	if (a == x) return 1;
	if (pair_p(a) && (contains(car(a), x) || contains(cdr(a), x)))
		return 1;
	return 0;
}

int liftable(cell x) {
	return !contains(x, S_set_b);
}

cell liftargs(cell m) {
	#define source	cadr
	cell	a, n;

	save(a = NIL);
	while (m != NIL) {
		if (caar(m) == I_a) {
			n = source(car(m));
			n = cons(n, NIL);
			n = cons(I_arg, n);
			a = cons(n, a);
			car(Stack) = a;
		}
		m = cdr(m);
	}
	return nreverse(unsave(1));
	#undef source
}

cell liftnames(cell m) {
	#define name cadddr
	cell	a, n;

	save(a = NIL);
	while (m != NIL) {
		if (caar(m) == I_a) {
			n = name(car(m));
			a = cons(n, a);
			car(Stack) = a;
		}
		m = cdr(m);
	}
	return nreverse(unsave(1));
	#undef name
}

/*
 * The following function is a mess. Here is what it does in
 * more readable Scheme:
 *
 * (define (app-conv x e a)
 *   (let ((fv   (free (car x)))
 *         (fn   (car x))
 *         (args (cdr x)))
 *     (for-each new-symbol! fv)
 *     (let ((m (initmap fv e a)))
 *       `((%closure ,(append (pick-vars m) (cadr fn))
 *                    #f
 *                    ,@(let ((ee (append (lift-names m)
 *                                        (flatten (cadr fn)))))
 *                        (map (lambda (x) (conv x e ee))
 *                             (cddr fn))))
 *         ,@(lift-args m)
 *         ,@(map (lambda (x) (conv x e a)) args)))))
 */

cell appconv(cell x, cell e, cell a) {
	cell	fv, fn, args, m, n, ce, vars, fnargs;

	fv = free_vars(car(x), NIL);
	save(fv);
	fn = car(x);
	args = cdr(x);
	fnargs = flatargs(cadr(fn));
	save(fnargs);
	newvars(fv);
	m = initmap(fv, e, a);
	save(m);
	args = mapconv(args, e, a);
	save(args);
	n = liftargs(m);
	args = nconc(n, args);
	car(Stack) = args;
	ce = liftnames(m);
	save(ce);
	vars = conc(ce, cadr(fn));
	save(vars);
	ce = set_union(ce, fnargs);
	cadr(Stack) = ce;
	fn = mapconv(cddr(fn), e, ce);
	fn = cons(NIL, fn);
	fn = cons(vars, fn);
	fn = cons(I_closure, fn);
	unsave(6);
	return cons(fn, args);
}

cell defconv(cell x, cell e, cell a) {
	cell	n, m;

	newvar(cadr(x));
	n = mapconv(cddr(x), e, a);
	save(n);
	m = mkfix(hashq(cadr(x), e));
	save(m);
	m = cons(I_ref, cons(m, cons(cadr(x), NIL)));
	unsave(2);
	return cons(S_set_b, cons(m, n));
}

cell cconv(cell x, cell e, cell a) {
	int	n, p;

	if (	pair_p(x) &&
		(S_apply == car(x)  ||
		 S_if == car(x)     ||
		 S_ifstar == car(x) ||
		 S_begin == car(x)  ||
		 S_set_b == car(x)  ||
		 subrp(car(x))))
	{
		return cons(car(x), mapconv(cdr(x), e, a));
	}
	if ((n = posq(x, a)) != FALSE) {
		return cons(I_arg, cons(mkfix(n), NIL));
	}
	if ((n = hashq(x, e)) != FALSE) {
		p = cons(x, NIL);
		n = cons(I_ref, cons(mkfix(n), p));
		return n;
	}
	if (symbol_p(x)) {
		error("undefined symbol", x);
		return NIL;
	}
	if (atom_p(x)) {
		return x;
	}
	if (S_quote == car(x)) {
		return x;
	}
	if (	pair_p(car(x)) &&
		caar(x) == S_lambda &&
		liftable(car(x)))
	{
		return appconv(x, e, a);
	}
	if (S_lambda == car(x)) {
		return lamconv(x, e, a);
	}
	if (S_define == car(x)) {
		return defconv(x, e, a);
	}
	if (S_define_syntax == car(x)) {
		return cons(car(x),
			    cons(cadr(x),
				 mapconv(cddr(x), e, a)));
	}
	return mapconv(x, e, a);
}

cell zipenv(cell vs, cell oe) {
	cell	n, b;

	save(n = NIL);
	while (vs != NIL) {
		if (NIL == oe) {
			b = cons(car(vs), cons(UNDEFINED, NIL));
		}
		else {
			b = cons(car(vs), cdar(oe));
			oe = cdr(oe);
		}
		n = cons(b, n);
		car(Stack) = n;
		vs = cdr(vs);
	}
	return nreverse(unsave(1));
}

cell clsconv(cell x) {
	cell	n;

	Env = Envp = NIL;
	Env = carof(Glob);
	if (NIL == Env) Env = cons(UNDEFINED, NIL);
	n = cconv(x, Env, NIL);
	save(n);
	Glob = zipenv(Env, Glob);
	return unsave(1);
}

/* Bytecode generation */

void emit(cell x) {
	cell	n, *vp, *vn;
	int	i, k;

	if (Here >= vector_len(Emitbuf)) {
		save(x);
		k = vector_len(Emitbuf);
		n = make_vector(CHUNK_SIZE + k);
		vp = vector(Emitbuf);
		vn = vector(n);
		for (i = 0; i < k; i++) vn[i] = vp[i];
		Emitbuf = n;
		unsave(1);
	}
	vector(Emitbuf)[Here] = x;
	Here++;
}

void emitop(cell op) {
	emit(mkfix(op));
}

void emitq(cell x) {
	emitop(OP_QUOTE);
	emit(x);
}

void patch(int a, cell x) {
	vector(Emitbuf)[a] = x;
}

cell cpop(void) {
	cell	n;

	if (NIL == Cts)
		error("oops: compile stack underflow", UNDEFINED);
	n = car(Cts);
	Cts = cdr(Cts);
	return n;
}

#define cpushval(x) 	(Cts = cons(mkfix(x), Cts))
#define cpopval()	fixval(cpop())

void swap(void) {
	cell	x;

	if (NIL == Cts || NIL == cdr(Cts))
		error("oops: compile stack underflow", UNDEFINED);
	x = car(Cts);
	car(Cts) = cadr(Cts);
	cadr(Cts) = x;
}

int subr0p(cell x) {
	if (x == P_command_line)	return OP_COMMAND_LINE;
	if (x == P_current_error_port)	return OP_CURRENT_ERROR_PORT;
	if (x == P_current_input_port)	return OP_CURRENT_INPUT_PORT;
	if (x == P_current_output_port)	return OP_CURRENT_OUTPUT_PORT;
	if (x == P_gensym) 		return OP_GENSYM;
	if (x == P_quit)		return OP_QUIT;
	if (x == P_symbols)		return OP_SYMBOLS;
	return -1;
}

int subr1p(cell x) {
	if (x == P_abs)			return OP_ABS;
	if (x == P_boolean_p)		return OP_BOOLEAN_P;
	if (x == P_caaaar)		return OP_CAAAAR;
	if (x == P_caaadr)		return OP_CAAADR;
	if (x == P_caaar)		return OP_CAAAR;
	if (x == P_caadar)		return OP_CAADAR;
	if (x == P_caaddr)		return OP_CAADDR;
	if (x == P_caadr)		return OP_CAADR;
	if (x == P_caar)		return OP_CAAR;
	if (x == P_cadaar)		return OP_CADAAR;
	if (x == P_cadadr)		return OP_CADADR;
	if (x == P_cadar)		return OP_CADAR;
	if (x == P_caddar)		return OP_CADDAR;
	if (x == P_cadddr)		return OP_CADDDR;
	if (x == P_caddr)		return OP_CADDR;
	if (x == P_cadr)		return OP_CADR;
	if (x == P_call_cc)		return OP_CALL_CC;
	if (x == P_call_with_current_continuation)
					return OP_CALL_CC;
	if (x == P_car)			return OP_CAR;
	if (x == P_catch)		return OP_CATCH;
	if (x == P_catch_tag_p)		return OP_CATCH_TAG_P;
	if (x == P_cdaaar)		return OP_CDAAAR;
	if (x == P_cdaadr)		return OP_CDAADR;
	if (x == P_cdaar)		return OP_CDAAR;
	if (x == P_cdadar)		return OP_CDADAR;
	if (x == P_cdaddr)		return OP_CDADDR;
	if (x == P_cdadr)		return OP_CDADR;
	if (x == P_cdar)		return OP_CDAR;
	if (x == P_cddaar)		return OP_CDDAAR;
	if (x == P_cddadr)		return OP_CDDADR;
	if (x == P_cddar)		return OP_CDDAR;
	if (x == P_cdddar)		return OP_CDDDAR;
	if (x == P_cddddr)		return OP_CDDDDR;
	if (x == P_cdddr)		return OP_CDDDR;
	if (x == P_cddr)		return OP_CDDR;
	if (x == P_cdr)			return OP_CDR;
	if (x == P_ceiling)		return OP_CEILING;
	if (x == P_char_alphabetic_p)	return OP_CHAR_ALPHABETIC_P;
	if (x == P_char_downcase)	return OP_CHAR_DOWNCASE;
	if (x == P_char_lower_case_p)	return OP_CHAR_LOWER_CASE_P;
	if (x == P_char_numeric_p)	return OP_CHAR_NUMERIC_P;
	if (x == P_char_p)		return OP_CHAR_P;
	if (x == P_char_to_integer)	return OP_CHAR_TO_INTEGER;
	if (x == P_char_upcase)		return OP_CHAR_UPCASE;
	if (x == P_char_upper_case_p)	return OP_CHAR_UPPER_CASE_P;
	if (x == P_char_whitespace_p)	return OP_CHAR_WHITESPACE_P;
	if (x == P_close_input_port)	return OP_CLOSE_INPUT_PORT;
	if (x == P_close_output_port)	return OP_CLOSE_OUTPUT_PORT;
	if (x == P_delete_file)		return OP_DELETE_FILE;
	if (x == P_dump_image)		return OP_DUMP_IMAGE;
	if (x == P_environment_variable)
					return OP_ENVIRONMENT_VARIABLE;
	if (x == P_eof_object_p)	return OP_EOF_OBJECT_P;
	if (x == P_eval)		return OP_EVAL;
	if (x == P_even_p)		return OP_EVEN_P;
	if (x == P_exact_p)		return OP_EXACT_P;
	if (x == P_exact_to_inexact)	return OP_EXACT_TO_INEXACT;
	if (x == P_exponent)		return OP_EXPONENT;
	if (x == P_file_exists_p)	return OP_FILE_EXISTS_P;
	if (x == P_floor)		return OP_FLOOR;
	if (x == P_inexact_p)		return OP_INEXACT_P;
	if (x == P_inexact_to_exact)	return OP_INEXACT_TO_EXACT;
	if (x == P_input_port_p)	return OP_INPUT_PORT_P;
	if (x == P_integer_p)		return OP_INTEGER_P;
	if (x == P_integer_to_char)	return OP_INTEGER_TO_CHAR;
	if (x == P_length)		return OP_LENGTH;
	if (x == P_list_to_string)	return OP_LIST_TO_STRING;
	if (x == P_list_to_vector)	return OP_LIST_TO_VECTOR;
	if (x == P_load)		return OP_LOAD;
	if (x == P_macro_expand)	return OP_MACRO_EXPAND;
	if (x == P_macro_expand_1)	return OP_MACRO_EXPAND_1;
	if (x == P_mantissa)		return OP_MANTISSA;
	if (x == P_negative_p)		return OP_NEGATIVE_P;
	if (x == P_not)			return OP_NOT;
	if (x == P_null_p)		return OP_NULL_P;
	if (x == P_number_p)		return OP_REAL_P;
	if (x == P_odd_p)		return OP_ODD_P;
	if (x == P_open_append_file)	return OP_OPEN_APPEND_FILE;
	if (x == P_open_input_file)	return OP_OPEN_INPUT_FILE;
	if (x == P_open_output_file)	return OP_OPEN_OUTPUT_FILE;
	if (x == P_output_port_p)	return OP_OUTPUT_PORT_P;
	if (x == P_pair_p)		return OP_PAIR_P;
	if (x == P_positive_p)		return OP_POSITIVE_P;
	if (x == P_procedure_p)		return OP_PROCEDURE_P;
	if (x == P_real_p)		return OP_REAL_P;
	if (x == P_reverse)		return OP_REVERSE;
	if (x == P_reverse_b)		return OP_REVERSE_B;
	if (x == P_s9_bytecode)		return OP_S9_BYTECODE;
	if (x == P_set_input_port_b)	return OP_SET_INPUT_PORT_B;
	if (x == P_set_output_port_b)	return OP_SET_OUTPUT_PORT_B;
	if (x == P_stats)		return OP_STATS;
	if (x == P_string_copy)		return OP_STRING_COPY;
	if (x == P_string_length)	return OP_STRING_LENGTH;
	if (x == P_string_p)		return OP_STRING_P;
	if (x == P_string_to_list)	return OP_STRING_TO_LIST;
	if (x == P_string_to_symbol)	return OP_STRING_TO_SYMBOL;
	if (x == P_symbol_p)		return OP_SYMBOL_P;
	if (x == P_symbol_to_string)	return OP_SYMBOL_TO_STRING;
	if (x == P_system_command)	return OP_SYSTEM_COMMAND;
	if (x == P_truncate)		return OP_TRUNCATE;
	if (x == P_vector_length)	return OP_VECTOR_LENGTH;
	if (x == P_vector_p)		return OP_VECTOR_P;
	if (x == P_vector_to_list)	return OP_VECTOR_TO_LIST;
	if (x == P_zero_p)		return OP_ZERO_P;
	return -1;
}

int subr2p(cell x) {
	if (x == P_assq)		return OP_ASSQ;
	if (x == P_assv)		return OP_ASSV;
	if (x == P_cons)		return OP_CONS;
	if (x == P_eq_p)		return OP_EQ_P;
	if (x == P_eqv_p)		return OP_EQV_P;
	if (x == P_list_ref)		return OP_LIST_REF;
	if (x == P_list_tail)		return OP_LIST_TAIL;
	if (x == P_memq)		return OP_MEMQ;
	if (x == P_memv)		return OP_MEMV;
	if (x == P_quotient)		return OP_QUOTIENT;
	if (x == P_remainder)		return OP_REMAINDER;
	if (x == P_set_car_b)		return OP_SET_CAR_B;
	if (x == P_set_cdr_b)		return OP_SET_CDR_B;
	if (x == P_string_fill_b)	return OP_STRING_FILL_B;
	if (x == P_string_ref)		return OP_STRING_REF;
	if (x == P_throw)		return OP_THROW;
	if (x == P_vector_fill_b)	return OP_VECTOR_FILL_B;
	if (x == P_vector_ref)		return OP_VECTOR_REF;
	return -1;
}

int subr3p(cell x) {
	if (x == P_string_set_b)	return OP_STRING_SET_B;
	if (x == P_substring)		return OP_SUBSTRING;
	if (x == P_vector_set_b)	return OP_VECTOR_SET_B;
	return -1;
}

int osubr0p(cell x) {
	if (x == P_peek_char)	return OP_PEEK_CHAR;
	if (x == P_read)	return OP_READ;
	if (x == P_read_char)	return OP_READ_CHAR;
	return -1;
}

int osubr1p(cell x) {
	if (x == P_display)		return OP_DISPLAY;
	if (x == P_error)		return OP_ERROR;
	if (x == P_make_string)		return OP_MAKE_STRING;
	if (x == P_make_vector)		return OP_MAKE_VECTOR;
	if (x == P_write)		return OP_WRITE;
	if (x == P_write_char)		return OP_WRITE_CHAR;
	return -1;
}

int osubr4p(cell x) {
	if (x == P_vector_copy)	return OP_VECTOR_COPY;
	return -1;
}

int lsubr0p(cell x) {
	if (x == P_append)		return OP_APPEND;
	if (x == P_bit_op)		return OP_BIT_OP;
	if (x == P_plus)		return OP_PLUS;
	if (x == P_string_append)	return OP_STRING_APPEND;
	if (x == P_times)		return OP_TIMES;
	if (x == P_vector_append)	return OP_VECTOR_APPEND;
	return -1;
}

int lsubr1p(cell x) {
	if (x == P_divide)	return OP_DIVIDE;
	if (x == P_max)		return OP_MAX;
	if (x == P_min)		return OP_MIN;
	if (x == P_minus)	return OP_MINUS;
	return -1;
}

int lsubr2p(cell x) {
	if (x == P_char_ci_equal_p)	return OP_CHAR_CI_EQUAL_P;
	if (x == P_char_ci_grtr_p)	return OP_CHAR_CI_GRTR_P;
	if (x == P_char_ci_gteq_p)	return OP_CHAR_CI_GTEQ_P;
	if (x == P_char_ci_less_p)	return OP_CHAR_CI_LESS_P;
	if (x == P_char_ci_lteq_p)	return OP_CHAR_CI_LTEQ_P;
	if (x == P_char_equal_p)	return OP_CHAR_EQUAL_P;
	if (x == P_char_grtr_p)		return OP_CHAR_GRTR_P;
	if (x == P_char_gteq_p)		return OP_CHAR_GTEQ_P;
	if (x == P_char_less_p)		return OP_CHAR_LESS_P;
	if (x == P_char_lteq_p)		return OP_CHAR_LTEQ_P;
	if (x == P_equal)		return OP_EQUAL;
	if (x == P_grtr)		return OP_GRTR;
	if (x == P_gteq)		return OP_GTEQ;
	if (x == P_less)		return OP_LESS;
	if (x == P_lteq)		return OP_LTEQ;
	if (x == P_string_ci_equal)	return OP_STRING_SI_EQUAL_P;
	if (x == P_string_ci_grtr)	return OP_STRING_SI_GRTR_P;
	if (x == P_string_ci_gteq)	return OP_STRING_SI_GTEQ_P;
	if (x == P_string_ci_less)	return OP_STRING_SI_LESS_P;
	if (x == P_string_ci_lteq)	return OP_STRING_SI_LTEQ_P;
	if (x == P_string_equal)	return OP_STRING_EQUAL_P;
	if (x == P_string_grtr)		return OP_STRING_GRTR_P;
	if (x == P_string_gteq)		return OP_STRING_GTEQ_P;
	if (x == P_string_less)		return OP_STRING_LESS_P;
	if (x == P_string_lteq)		return OP_STRING_LTEQ_P;
	return -1;
}

int subrp(cell x) {
	return	subr0p(x) >= 0 ||
		subr1p(x) >= 0 ||
		subr2p(x) >= 0 ||
		subr3p(x) >= 0 ||
		osubr0p(x) >= 0 ||
		osubr1p(x) >= 0 ||
		osubr4p(x) >= 0 ||
		lsubr0p(x) >= 0 ||
		lsubr1p(x) >= 0 ||
		lsubr2p(x) >= 0;
}

void compexpr(cell x, int t);

void compbegin(cell x, int t) {
	x = cdr(x);
	if (NIL == x) {
		emitq(UNSPECIFIC);
		return;
	}
	while (cdr(x) != NIL) {
		compexpr(car(x), 0);
		x = cdr(x);
	}
	compexpr(car(x), t);
}

void compsetb(cell x) {
	compexpr(caddr(x), 0);
	if (caadr(x) == I_ref) {
		emitop(OP_SET_REF);
		emit(cadadr(x));
	}
	else if (caadr(x) == I_arg) {
		emitop(OP_SET_ARG);
		emit(cadadr(x));
	}
	else {
		error("oops: unknown location in set!", x);
	}
}

void compif(cell x, int t, int star) {
	compexpr(cadr(x), 0);
	emitop(star? OP_JMP_TRUE: OP_JMP_FALSE);
	cpushval(Here);
	emit(NIL);
	compexpr(caddr(x), t);
	if (cdddr(x) != NIL) {
		emitop(OP_JMP);
		cpushval(Here);
		emit(NIL);
		swap();
		patch(cpopval(), mkfix(Here));
		compexpr(cadddr(x), t);
	}
	patch(cpopval(), mkfix(Here));
}

void setupenv(cell m) {
	while (m != NIL) {
		if (caar(m) == I_e)
			emitop(OP_COPY_REF);
		else if (caar(m) == I_a)
			emitop(OP_COPY_ARG);
		else
			error("oops: unknown location in closure", m);
		emit(cadar(m));
		emit(caddar(m));
		m = cdr(m);
	}
}

void compcls(cell x) {
	int	a, na;
	cell	b, m;

	emitop(OP_JMP);
	cpushval(Here);
	emit(NIL);
	a = Here;
	na = length(flatargs(cadr(x)));
	if (dotted_p(cadr(x))) {
		emitop(OP_ENTER_COLL);
		emit(mkfix(na-1));
	}
	else {
		emitop(OP_ENTER);
		emit(mkfix(na));
	}
	b = cons(S_begin, cdddr(x));
	save(b);
	compexpr(b, 1);
	unsave(1);
	emitop(OP_RETURN);
	patch(cpopval(), mkfix(Here));
	m = caddr(x);
	if (m != NIL) {
		emitop(OP_MAKE_ENV);
		emit(mkfix(length(m)));
		setupenv(m);
	}
	else {
		emitop(OP_PROP_ENV);
	}
	emitop(OP_CLOSURE);
	emit(mkfix(a));
}

void compapply(cell x, int t) {
	cell	xs;

	xs = reverse(cddr(x));
	save(xs);
	compexpr(car(xs), 0);
	for (xs = cdr(xs); xs != NIL; xs = cdr(xs)) {
		emitop(OP_PUSH);
		compexpr(car(xs), 0);
		emitop(OP_CONS);
	}
	emitop(OP_PUSH);
	unsave(1);
	compexpr(cadr(x), 0);
	emitop(t? OP_TAIL_APPLIS: OP_APPLIS);
}

void compapp(cell x, int t) {
	cell	xs;

	xs = reverse(cdr(x));
	save(xs);
	while (xs != NIL) {
		compexpr(car(xs), 0);
		emitop(OP_PUSH);
		xs = cdr(xs);
	}
	unsave(1);
	emitop(OP_PUSH_VAL);
	emit(mkfix(length(cdr(x))));
	compexpr(car(x), 0);
	emitop(t? OP_TAIL_APPLY: OP_APPLY);
}

void compsubr0(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 0, 0);
	emitop(op);
}

void compsubr1(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 1, 1);
	compexpr(cadr(x), 0);
	emitop(op);
	if (OP_CALL_CC == op || OP_CATCH == op)
		emitop(OP_APPLY);
}

void compsubr2(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 2, 2);
	compexpr(caddr(x), 0);
	emitop(OP_PUSH);
	compexpr(cadr(x), 0);
	emitop(op);
}

void compsubr3(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 3, 3);
	compexpr(cadddr(x), 0);
	emitop(OP_PUSH);
	compexpr(caddr(x), 0);
	emitop(OP_PUSH);
	compexpr(cadr(x), 0);
	emitop(op);
}

void composubr0(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 0, 1);
	if (NIL == cdr(x))
		emitop(OP_CURRENT_INPUT_PORT);
	else
		compexpr(cadr(x), 0);
	emitop(op);
}

void composubr1(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 1, 2);
	if (NIL == cddr(x)) {
		if (OP_ERROR == op) {
			/**/
		}
		if (OP_MAKE_STRING == op) {
			emitop(OP_QUOTE);
			emit(make_char(' '));
		}
		if (OP_MAKE_VECTOR == op) {
			emitq(FALSE);
		}
		if (	OP_DISPLAY == op ||
			OP_WRITE == op ||
			OP_WRITE_CHAR == op)
		{
			emitop(OP_CURRENT_OUTPUT_PORT);
		}
	}
	else {
		if (OP_ERROR == op) op = OP_ERROR2;
		compexpr(caddr(x), 0);
	}
	emitop(OP_PUSH);
	compexpr(cadr(x), 0);
	emitop(op);
}

void composubr4(cell x, int op) {
	int	k;

	ckargs(x, symbol_name(car(x)), 1, 4);
	x = cdr(x);
	k = length(x);
	if (k < 4)
		emitq(FALSE);
	else
		compexpr(cadddr(x), 0);
	emitop(OP_PUSH);
	if (k < 3)
		emitq(UNDEFINED);
	else
		compexpr(caddr(x), 0);
	emitop(OP_PUSH);
	if (k < 2)
		emitq(UNDEFINED);
	else
		compexpr(cadr(x), 0);
	emitop(OP_PUSH);
	compexpr(car(x), 0);
	emitop(op);
}

void complsubr0(cell x, int op) {
	cell	bitop = 0; /*LINT*/

	if (OP_BIT_OP == op && length(x) < 4)
		ckargs(x, "bit-op", 3, -1);
	if (NIL == cdr(x)) {
		if (OP_PLUS == op) {
			emitq(Zero);
		}
		else if (OP_TIMES == op) {
			emitq(One);
		}
		else if (OP_STRING_APPEND == op) {
			emitop(OP_QUOTE);
			emit(make_string("", 0));
		}
		else if (OP_VECTOR_APPEND == op) {
			emitop(OP_QUOTE);
			emit(make_vector(0));
		}
		else { /* OP_APPEND */
			emitq(NIL);
		}
	}
	else if (NIL == cddr(x)) {
		compexpr(cadr(x), 0);
	}
	else {
		x = cdr(x);
		if (OP_BIT_OP == op) {
			bitop = car(x);
			x = cdr(x);
		}
		x = reverse(x);
		save(x);
		compexpr(car(x), 0);
		x = cdr(x);
		while (x != NIL) {
			emitop(OP_PUSH);
			compexpr(car(x), 0);
			if (OP_BIT_OP == op) {
				emitop(OP_PUSH);
				compexpr(bitop, 0);
			}
			emitop(op);
			x = cdr(x);
		}
		unsave(1);
	}
}

void complsubr1(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 1, -1);
	if (NIL == cddr(x)) {
		if (OP_MINUS == op) {
			compexpr(cadr(x), 0);
			emitop(OP_NEGATE);
		}
		else if (OP_DIVIDE == op) {
			emitq(One);
			emitop(OP_PUSH);
			compexpr(cadr(x), 0);
			emitop(op);
		}
		else { /* OP_MIN, OP_MAX */
			compexpr(cadr(x), 0);
		}
	}
	else {
		if (OP_MIN == op || OP_MAX == op) {
			emitop(OP_PUSH_VAL);
			emit(FALSE);
		}
		x = cdr(x);
		compexpr(car(x), 0);
		for (x = cdr(x); x != NIL; x = cdr(x)) {
			emitop(OP_PUSH);
			compexpr(car(x), 0);
			emitop(op);
		}
		if (OP_MIN == op || OP_MAX == op) {
			emitop(OP_FIX_EXACTNESS);
		}
	}
}

void complsubr2(cell x, int op) {
	ckargs(x, symbol_name(car(x)), 2, -1);
	emitop(OP_PUSH_VAL);
	emit(TRUE);
	x = cdr(x);
	compexpr(car(x), 0);
	for (x = cdr(x); x != NIL; x = cdr(x)) {
		emitop(OP_PUSH);
		compexpr(car(x), 0);
		emitop(op);
	}
	emitop(OP_POP);
}

void compexpr(cell x, int t) {
	int	op;

	if (atom_p(x)) {
		emitq(x);
	}
	else if (car(x) == S_quote) {
		emitq(cadr(x));
	}
	else if (car(x) == I_arg) {
		emitop(OP_ARG);
		emit(cadr(x));
	}
	else if (car(x) == I_ref) {
		emitop(OP_REF);
		emit(cadr(x));
		emit(caddr(x));
	}
	else if (car(x) == S_if) {
		compif(x, t, 0);
	}
	else if (car(x) == S_ifstar) {
		compif(x, t, 1);
	}
	else if (car(x) == I_closure) {
		compcls(x);
	}
	else if (car(x) == S_begin) {
		compbegin(x, t);
	}
	else if (car(x) == S_set_b) {
		compsetb(x);
	}
	else if (car(x) == S_apply) {
		compapply(x, t);
	}
	else if (car(x) == S_define_syntax) {
		compexpr(caddr(x), 0);
		emitop(OP_DEF_MACRO);
		emit(cadr(x));
	}
	else if ((op = subr0p(car(x))) >= 0) {
		compsubr0(x, op);
	}
	else if ((op = subr1p(car(x))) >= 0) {
		compsubr1(x, op);
	}
	else if ((op = subr2p(car(x))) >= 0) {
		compsubr2(x, op);
	}
	else if ((op = subr3p(car(x))) >= 0) {
		compsubr3(x, op);
	}
	else if ((op = osubr0p(car(x))) >= 0) {
		composubr0(x, op);
	}
	else if ((op = osubr1p(car(x))) >= 0) {
		composubr1(x, op);
	}
	else if ((op = osubr4p(car(x))) >= 0) {
		composubr4(x, op);
	}
	else if ((op = lsubr0p(car(x))) >= 0) {
		complsubr0(x, op);
	}
	else if ((op = lsubr1p(car(x))) >= 0) {
		complsubr1(x, op);
	}
	else if ((op = lsubr2p(car(x))) >= 0) {
		complsubr2(x, op);
	}
	else { /* application */
		compapp(x, t);
	}
}

cell compile(cell x) {
	Emitbuf = make_vector(CHUNK_SIZE);
	Here = 0;
	Cts = NIL;
	compexpr(x, 0);
	emitop(OP_HALT);
	return subvector(Emitbuf, 0, Here);
}

/*
 * Macro expander
 */

cell expand(cell x, int all);

cell mapexp(cell x, int all) {
	cell	p, n, new;

	save(x);
	save(n = NIL);
	for (p = x; pair_p(p); p = cdr(p)) {
		new = expand(car(p), all);
		n = cons(new, n);
		car(Stack) = n;
	}
	if (p != NIL) error("improper list in program", x);
	n = nreverse(unsave(1));
	unsave(1);
	return n;
}

cell expanddef(cell x) {
	cell	n;

	save(x);
	n = cons(cdadr(x), cddr(x));
	n = cons(S_lambda, n);
	n = cons(n, NIL);
	n = cons(caadr(x), n);
	n = cons(car(x), n);
	unsave(1);
	return n;
}

cell expandbody(cell x) {
	cell	n, vs, as;

	save(x);
	save(vs = NIL);
	save(as = NIL);
	while (	pair_p(x) &&
		pair_p(car(x)) &&
		caar(x) == S_define)
	{
		n = car(x);
		if (pair_p(cadr(n)))
			n = expanddef(n);
		save(n);
		vs = cons(cadr(n), vs);
		caddr(Stack) = vs;
		n = cons(caddr(n), NIL);
		as = cons(n, as);
		cadr(Stack) = as;
		unsave(1);
		x = cdr(x);
	}
	if (NIL == vs) {
		unsave(3);
		return x;
	}
	as = nreverse(as);
	car(Stack) = as;
	vs = nreverse(vs);
	cadr(Stack) = vs;
	n = cons(zip(vs, as), x);
	n = cons(S_letrec, n);
	n = cons(n, NIL);
	unsave(3);
	return n;
}

cell expand(cell x, int all) {
	cell	n, m;

	if (Expand_level < 0) error("interrupted", UNDEFINED);
	if (Expand_level > MAX_EXPAND)
		error("too many levels of macro expansion", UNDEFINED);
	Expand_level++;
	if (atom_p(x)) {
		Expand_level--;
		return x;
	}
	if (car(x) == S_quote) {
		Expand_level--;
		return x;
	}
	if (car(x) == S_lambda) {
		save(x);
		n = expandbody(cddr(x));
		n = mapexp(n, all);
		n = cons(cadr(x), n);
		unsave(1);
		Expand_level--;
		return cons(car(x), n);
	}
	if (	car(x) == S_define ||
		car(x) == S_define_syntax)
	{
		save(x);
		if (pair_p(cadr(x))) {
			n = expanddef(x);
			car(Stack) = n;
			n = expand(n, all);
		}
		else {
			n = expand(caddr(x), all);
			n = cons(n, NIL);
			n = cons(cadr(x), n);
			n = cons(car(x), n);
		}
		unsave(1);
		Expand_level--;
		return n;
	}
	if ((m = assq(car(x), Macros)) != FALSE) {
		save(x);
		n = cons(cdr(x), NIL);
		n = cons(S_quote, n);
		n = cons(n, NIL);
		n = cons(cdr(m), n);
		n = cons(S_apply, n);
		n = eval(n);
		if (all) {
			save(n);
			n = expand(n, all);
			unsave(1);
		}
		unsave(1);
		Expand_level--;
		return n;
	}
	x = mapexp(x, all);
	Expand_level--;
	return x;
}

/*
 * Abstract machine
 */

#define ins()		fixval(vector(Prog)[Ip])
#define op1()		(vector(Prog)[Ip+1])
#define op2()		(vector(Prog)[Ip+2])

#define skip(n)		(Ip += (n))

#define box(x)		cons((x), NIL)
#define boxref(x)	car(x)
#define boxset(x,v)	(car(x) = (v))

#define stackref(x)	(vector(Rts)[x])
#define stackset(x,v)	(vector(Rts)[x] = (v))

#define argbox(n)	(stackref(Fp-fixval(n)))
#define argref(n)	boxref(argbox(n))

#define arg(n)		boxref(stackref(Sp-(n)))

#define clear(n)	(Sp -= (n))

#define envbox(x)	(vector(Ep)[fixval(x)])

void stkalloc(int k) {
	cell	n, *vs, *vn;
	int	i;

	if (Sp + k >= Sz) {
		n = make_vector(Sz + CHUNK_SIZE);
		vs = vector(Rts);
		vn = vector(n);
		for (i=0; i<Sz; i++) vn[i] = vs[i];
		Sz += CHUNK_SIZE;
		Rts = n;
	}
}

void push(cell x) {
	Tmp = x;
	stkalloc(1);
	Tmp = NIL;
	Sp++;
	stackset(Sp, x);
}

/* Opcodes */

cell apply_extproc(cell pfn) {
	cell	a, x;
	int	i, k;
	char	*s;

	save(a = NIL);
	k = fixval(stackref(Sp));
	for (i = k; i > 0; i--) {
		a = cons(car(stackref(Sp-i)), a);
		car(Stack) = a;
	}
	s = typecheck(pfn, a);
	if (s != NULL) error(s, a);
	x = apply_prim(pfn, a);
	unsave(1);
	Sp -= k+1;
	return x;
}

int apply(int tail) {
	int	n, m, pn, pm, i;
	cell	k, e;

	if (!function_p(Acc)) {
		if (primitive_p(Acc)) {
			Acc = apply_extproc(Acc);
			return Ip+1;
		}
		if (continuation_p(Acc)) {
			return call_cont(Acc, arg(1));
		}
		error("application of non-function", Acc);
	}
	if (tail) {
		Ep = closure_env(Acc);
		Prog = closure_prog(Acc);
		m = fixval(stackref(Sp));
		n = fixval(stackref(Sp-m-4));
		pm = Sp-m;
		pn = Sp-m-n-4;
		if (n == m) {
			for (i=0; i<=m; i++)
				stackset(pn+i, stackref(pm+i));
			Fp = fixval(stackref(Sp-m-1));
			Sp -= n+2;
		}
		else {
			e = stackref(Sp-m-3);
			k = stackref(Sp-m-2);
			Fp = fixval(stackref(Sp-m-1));
			for (i=0; i<=m; i++)
				stackset(pn+i, stackref(pm+i));
			Sp -= n+2;
			stackset(Sp-1, e);
			stackset(Sp,   k);
		}
	}
	else {
		push(Ep);
		push(cons(mkfix(Ip+1), Prog));
		Ep = closure_env(Acc);
		Prog = closure_prog(Acc);
	}
	return fixval(closure_ip(Acc));
}

int applis(int tail) {
	cell	a, p, new;
	int	k, i;

	a = arg(0);
	if (!list_p(a)) error("apply: expected list", a);
	k = conses(a);
	stkalloc(k);
	Sp += k;
	i = Sp-1;
	for (p = a; p != NIL; p = cdr(p)) {
		if (atom_p(p)) error("apply: improper list", a);
		new = box(car(p));
		stackset(i, new);
		i--;
	}
	new = mkfix(k);
	stackset(Sp, new);
	return apply(tail);
}

int ret(void) {
	int	r, k;
	cell	*v;

	v = vector(Rts);
	Fp = fixval(v[Sp]);
	r = v[Sp-1];
	Prog = cdr(r);
	Ep = v[Sp-2];
	k = fixval(v[Sp-3]);
	Sp -= k+4;
	return fixval(car(r));
}

void entcol(int fix) {
	int	n, na, i, s, d;
	cell	a, x, new;

	na = fixval(stackref(Sp-2));
	if (na < fix)
		error("too few arguments", UNDEFINED);
	save(a = NIL);
	i = Sp-fix-3;
	for (n = na-fix; n; n--) {
		x = cons(boxref(stackref(i)), NIL);
		if (NIL == a) {
			a = x;
			car(Stack) = a;
		}
		else {
			cdr(a) = x;
			a = x;
		}
		i--;
	}
	a = unsave(1);
	if (na > fix) {
		new = box(a);
		stackset(Sp-fix-3, new);
	}
	else {
		push(NIL);
		s = Sp - na - 3;
		d = Sp - na - 2;
		for (i = na + 2; i >= 0; i--)
			stackset(d+i, stackref(s+i));
		new = mkfix(1+fix);
		stackset(Sp-2, new);
		new = box(NIL);
		stackset(Sp-fix-3, new);
	}
	push(mkfix(Fp));
	Fp = Sp-4;
}

void newmacro(cell name, cell fn) {
	cell	n;

	n = assq(name, Macros);
	if (FALSE == n) {
		n = cons(name, fn);
		Macros = cons(n, Macros);
	}
	else {
		cdr(n) = fn;
	}
}

/* Inlined primitive procedures */

cell integer_value(char *who, cell x) {
	char	msg[100];

	if (!integer_p(x)) {
		sprintf(msg, "%s: expected integer, got", who);
		error(msg, x);
		return 0;
	}
	if (cddr(x) != NIL) {
		sprintf(msg, "%s: integer argument too big", who);
		error(msg, x);
		return 0;
	}
	return cadr(x);
}

cell integer_argument(char *who, cell x) {
	cell	n;
	char	msg[100];

	if (real_p(x)) {
		n = real_to_bignum(x);
		if (UNDEFINED == n) {
			sprintf(msg, "%s: expected integer, got", who);
			error(msg, x);
			return UNDEFINED;
		}
		return n;
	}
	return x;
}

cell gensym(void) {
	static int	id = 0;
	char		b[100];
	cell		p;

	if (0 == id) {
		for (p = Glob; p != NIL; p = cdr(p))
			if ('G' == symbol_name(caar(p))[0])
				id = asctol(&symbol_name(caar(p))[1]);
	}
	id++;
	sprintf(b, "G%d", id);
	return symbol_ref(b);
}

char *rev_cxr_name(char *s) {
	int		i, k = strlen(s);
	static char	buf[8];

	for (i=0; i<k; i++) buf[i] = s[k-i-1];
	buf[i] = 0;
	return buf;
}

cell cxr(char *op, cell x) {
	char	*p;
	cell	n = x;
	char	buf[64];

	for (p = op; *p; p++) {
		if (!pair_p(n)) {
			sprintf(buf, "c%sr: unsuitable type for operation",
				rev_cxr_name(op));
			error(buf, x);
		}
		n = 'a' == *p? car(n): cdr(n);
	}
	return n;
}

cell append(cell a, cell b) {
	cell	n, p, pn, new;

	if (NIL == a) return b;
	if (NIL == b) return a;
	save(n = cons(NIL, NIL));
	for (p = a; pair_p(p); p = cdr(p)) {
		car(n) = car(p);
		pn = n;
		if (pair_p(cdr(p))) {
			new = cons(NIL, NIL);
			cdr(n) = new;
			n = new;
		}
	}
	if (p != NIL) error("append: improper list", a);
	cdr(pn) = b;
	return unsave(1);
}

cell list_length(cell a) {
	cell	p;
	int	k;

	k = 0;
	for (p = a; pair_p(p); p = cdr(p))
		k++;
	if (p != NIL) error("length: improper list", a);
	return make_integer(k);
}

int eqv_p(cell a, cell b) {
	if (a == b) return 1;
	if (char_p(a) && char_p(b) && char_value(a) == char_value(b))
		return 1;
	return	number_p(a) &&
		number_p(b) &&
		real_p(a) == real_p(b) &&
		real_equal_p(a, b);
}

int assqv(char *who, int v, cell x, cell a) {
	cell	p;
	char	buf[64];

	for (p = a; p != NIL; p = cdr(p)) {
		if (!pair_p(p)) {
			sprintf(buf, "%s: improper list", who);
			error(buf, a);
		}
		if (!pair_p(car(p))) {
			sprintf(buf, "%s: bad element in alist", who);
			error(buf, car(p));
		}
		if (!v && x == caar(p))
			return car(p);
		if (v && eqv_p(x, caar(p)))
			return car(p);
	}
	return FALSE;
}

int memqv(char *who, int v, cell x, cell a) {
	cell	p;
	char	buf[64];

	for (p = a; p != NIL; p = cdr(p)) {
		if (!pair_p(p)) {
			sprintf(buf, "%s: improper list", who);
			error(buf, a);
		}
		if (!v && x == car(p))
			return p;
		if (v && eqv_p(x, car(p)))
			return p;
	}
	return FALSE;
}

int nth(char *who, int ref, cell a, cell n) {
	cell	p;
	char	buf[64];
	int	k;

	k = integer_value(who, n);
	for (p = a; p != NIL; p = cdr(p)) {
		if (0 == k) break;
		if (!pair_p(p)) {
			sprintf(buf, "%s: improper list", who);
			error(buf, a);
		}
		k--;
	}
	if (0 == k) {
		if (0 == ref)
			return p;
		else if (pair_p(p))
			return car(p);
	}
	sprintf(buf, "%s: index out of range", who);
	error(buf, n);
	return NIL;
}

cell bit_op(cell op, cell x, cell y) {
	char		name[] = "bit-op";
	cell		a, b;
	static cell	mask = 0;

	if (0 == mask) {
		mask = 1;
		while (mask < S9_INT_SEG_LIMIT)
			mask <<= 1;
		if (mask >= S9_INT_SEG_LIMIT)
			mask >>= 1;
		mask--;
	}
	op = integer_value(name, op);
	a = integer_value(name, x);
	b = integer_value(name, y);
	if ((a & ~mask) != 0) error("bit-op: range error", x);
	if ((b & ~mask) != 0) error("bit-op: range error", y);
	switch (op) {
	case  0: a =  0;        break;
	case  1: a =   a &  b;  break;
	case  2: a =   a & ~b;  break;
	case  3: /* a =   a; */ break;
	case  4: a =  ~a &  b;  break;
	case  5: a =        b;  break;
	case  6: a =   a ^  b;  break;
	case  7: a =   a |  b;  break;
	case  8: a = ~(a |  b); break;
	case  9: a = ~(a ^  b); break;
	case 10: a =       ~b;  break;
	case 11: a =   a | ~b;  break;
	case 12: a =  ~a;       break;
	case 13: a =  ~a |  b;  break;
	case 14: a = ~(a &  b); break;
	case 15: a = ~0;        break;
	case 16: a = a  <<  b;  break;
	case 17: a = a  >>  b;  break;
	default: error("bit-op: unknown opcode", op);
				break;
	}
	return make_integer(a & mask);
}

cell add(cell x, cell y) {
	if (!number_p(x)) expect("+", "number", x);
	if (!number_p(y)) expect("+", "number", y);
	return real_add(x, y);
}

cell xsub(cell x, cell y) {
	if (!number_p(x)) expect("-", "number", x);
	if (!number_p(y)) expect("-", "number", y);
	return real_subtract(y, x);
}

cell mul(cell x, cell y) {
	if (!number_p(x)) expect("*", "number", x);
	if (!number_p(y)) expect("*", "number", y);
	return real_multiply(x, y);
}

cell xdiv(cell x, cell y) {
	if (!number_p(x)) expect("/", "number", x);
	if (!number_p(y)) expect("/", "number", y);
	return real_divide(y, x);
}

cell intdiv(cell x, cell y) {
	x = integer_argument("quotient", x);
	save(x);
	y = integer_argument("quotient", y);
	unsave(1);
	if (bignum_zero_p(y)) error("quotient: divide by zero", UNDEFINED);
	x = bignum_divide(x, y);
	return car(x);
}

cell intrem(cell x, cell y) {
	x = integer_argument("remainder", x);
	save(x);
	y = integer_argument("remainder", y);
	unsave(1);
	if (bignum_zero_p(y)) error("remainder: divide by zero", UNDEFINED);
	x = bignum_divide(x, y);
	return cdr(x);
}

cell exact_to_inexact(cell x) {
	cell	n;
	int	flags;

	if (integer_p(x)) {
		flags = bignum_negative_p(x)? REAL_NEGATIVE: 0;
		n = bignum_abs(x);
		n = Make_real(flags, 0, cdr(n));
		if (UNDEFINED == n) error("exact->inexact: overflow", x);
		return n;
	}
	return x;
}

cell inexact_to_exact(cell x) {
	cell	n;

	if (integer_p(x)) return x;
	n = real_to_bignum(x);
	if (UNDEFINED == n)
		error("inexact->exact: no exact representation", x);
	return n;
}

void grtr(cell x, cell y) {
	if (!number_p(x)) expect(">", "number", x);
	if (!number_p(y)) expect(">", "number", y);
	if (!real_less_p(x, y)) stackset(Sp-1, FALSE);
}

void gteq(cell x, cell y) {
	if (!number_p(x)) expect(">=", "number", x);
	if (!number_p(y)) expect(">=", "number", y);
	if (real_less_p(y, x)) stackset(Sp-1, FALSE);
}

void less(cell x, cell y) {
	if (!number_p(x)) expect("<", "number", x);
	if (!number_p(y)) expect("<", "number", y);
	if (!real_less_p(y, x)) stackset(Sp-1, FALSE);
}

void lteq(cell x, cell y) {
	if (!number_p(x)) expect("<=", "number", x);
	if (!number_p(y)) expect("<=", "number", y);
	if (real_less_p(x, y)) stackset(Sp-1, FALSE);
}

void equal(cell x, cell y) {
	if (!number_p(x)) expect("=", "number", x);
	if (!number_p(y)) expect("=", "number", y);
	if (!real_equal_p(x, y)) stackset(Sp-1, FALSE);
}

void cless(cell x, cell y) {
	if (!char_p(x)) expect("char<?", "char", x);
	if (!char_p(y)) expect("char<?", "char", y);
	if (char_value(y) >= char_value(x)) stackset(Sp-1, FALSE);
}

void clteq(cell x, cell y) {
	if (!char_p(x)) expect("char<=?", "char", x);
	if (!char_p(y)) expect("char<=?", "char", y);
	if (char_value(y) > char_value(x)) stackset(Sp-1, FALSE);
}

void cequal(cell x, cell y) {
	if (!char_p(x)) expect("char=?", "char", x);
	if (!char_p(y)) expect("char=?", "char", y);
	if (char_value(y) != char_value(x)) stackset(Sp-1, FALSE);
}

void cgrtr(cell x, cell y) {
	if (!char_p(x)) expect("char>?", "char", x);
	if (!char_p(y)) expect("char>?", "char", y);
	if (char_value(y) <= char_value(x)) stackset(Sp-1, FALSE);
}

void cgteq(cell x, cell y) {
	if (!char_p(x)) expect("char>=?", "char", x);
	if (!char_p(y)) expect("char>=?", "char", y);
	if (char_value(y) < char_value(x)) stackset(Sp-1, FALSE);
}

void ciless(cell x, cell y) {
	if (!char_p(x)) expect("char<?", "char", x);
	if (!char_p(y)) expect("char<?", "char", y);
	if (tolower(char_value(y)) >= tolower(char_value(x)))
		stackset(Sp-1, FALSE);
}

void cilteq(cell x, cell y) {
	if (!char_p(x)) expect("char<=?", "char", x);
	if (!char_p(y)) expect("char<=?", "char", y);
	if (tolower(char_value(y)) > tolower(char_value(x)))
		stackset(Sp-1, FALSE);
}

void ciequal(cell x, cell y) {
	if (!char_p(x)) expect("char=?", "char", x);
	if (!char_p(y)) expect("char=?", "char", y);
	if (tolower(char_value(y)) != tolower(char_value(x)))
		stackset(Sp-1, FALSE);
}

void cigrtr(cell x, cell y) {
	if (!char_p(x)) expect("char>?", "char", x);
	if (!char_p(y)) expect("char>?", "char", y);
	if (tolower(char_value(y)) <= tolower(char_value(x)))
		stackset(Sp-1, FALSE);
}

void cigteq(cell x, cell y) {
	if (!char_p(x)) expect("char>=?", "char", x);
	if (!char_p(y)) expect("char>=?", "char", y);
	if (tolower(char_value(y)) < tolower(char_value(x)))
		stackset(Sp-1, FALSE);
}

void sless(cell x, cell y) {
	if (!string_p(x)) expect("string<?", "string", x);
	if (!string_p(y)) expect("string<?", "string", y);
	if (strcmp(string(y), string(x)) >= 0) stackset(Sp-1, FALSE);
}

void slteq(cell x, cell y) {
	if (!string_p(x)) expect("string<=?", "string", x);
	if (!string_p(y)) expect("string<=?", "string", y);
	if (strcmp(string(y), string(x)) > 0) stackset(Sp-1, FALSE);
}

void sequal(cell x, cell y) {
	if (!string_p(x)) expect("string=?", "string", x);
	if (!string_p(y)) expect("string=?", "string", y);
	if (string_len(x) != string_len(y)) {
		stackset(Sp-1, FALSE);
		return;
	}
	if (strcmp(string(y), string(x)) != 0) stackset(Sp-1, FALSE);
}

void sgrtr(cell x, cell y) {
	if (!string_p(x)) expect("string>?", "string", x);
	if (!string_p(y)) expect("string>?", "string", y);
	if (strcmp(string(y), string(x)) <= 0) stackset(Sp-1, FALSE);
}

void sgteq(cell x, cell y) {
	if (!string_p(x)) expect("string>=?", "string", x);
	if (!string_p(y)) expect("string>=?", "string", y);
	if (strcmp(string(y), string(x)) < 0) stackset(Sp-1, FALSE);
}

void siless(cell x, cell y) {
	if (!string(x)) expect("string-ci<?", "string", x);
	if (!string(y)) expect("string-ci<?", "string", y);
	if (strcmp_ci(string(y), string(x)) >= 0) stackset(Sp-1, FALSE);
}

void silteq(cell x, cell y) {
	if (!string_p(x)) expect("string-ci<=?", "string", x);
	if (!string_p(y)) expect("string-ci<=?", "string", y);
	if (strcmp_ci(string(y), string(x)) > 0) stackset(Sp-1, FALSE);
}

void siequal(cell x, cell y) {
	if (!string_p(x)) expect("string-ci=?", "string", x);
	if (!string_p(y)) expect("string-ci=?", "string", y);
	if (string_len(x) != string_len(y)) {
		stackset(Sp-1, FALSE);
		return;
	}
	if (strcmp_ci(string(y), string(x)) != 0) stackset(Sp-1, FALSE);
}

void sigrtr(cell x, cell y) {
	if (!string_p(x)) expect("string-ci>?", "string", x);
	if (!string_p(y)) expect("string-ci>?", "string", y);
	if (strcmp_ci(string(y), string(x)) <= 0) stackset(Sp-1, FALSE);
}

void sigteq(cell x, cell y) {
	if (!string_p(x)) expect("string-ci>=?", "string", x);
	if (!string_p(y)) expect("string-ci>=?", "string", y);
	if (strcmp_ci(string(y), string(x)) < 0) stackset(Sp-1, FALSE);
}

cell makestr(cell z, cell a) {
	cell	n;
	int	i, c, k;
	char	*s;

	k = integer_value("make-string", z);
	if (!char_p(a)) expect("make-string", "char", a);
	c = char_value(a);
	n = make_string("", k);
	s = string(n);
	for (i=0; i<k; i++) s[i] = c;
	return n;
}

cell sref(cell s, cell n) {
	int	i;

	if (!string_p(s)) expect("string-ref", "string", s);
	i = integer_value("string-ref", n);
	if (i < 0 || i >= string_len(s)-1)
		error("sref: index out of range", n);
	return make_char(string(s)[i]);
}

void sset(cell s, cell n, cell r) {
	int	i;

	if (!string_p(s)) expect("string-set!", "string", s);
	if (constant_p(s)) error("string-set!: immutable", s);
	i = integer_value("string-set!", n);
	if (!char_p(r)) expect("string-set!", "char", r);
	if (i < 0 || i >= string_len(s)-1)
		error("string-set!: index out of range", n);
	string(s)[i] = char_value(r);
}

cell substring(cell s, cell n0, cell n1) {
	int	k, k0, k1, i, j;
	cell	n;
	char	*s0, *s1;

	if (!string_p(s)) expect("substring", "string", s);
	k0 = integer_value("substring", n0);
	k1 = integer_value("substring", n1);
	if (k0 < 0 || k1 < 0 || k0 > k1 || k1 >= string_len(s))
		error("substring: invalid range", cons(n0, cons(n1, NIL)));
	k = k1-k0;
	n = make_string("", k);
	j = 0;
	s0 = string(s);
	s1 = string(n);
	for (i=k0; i<k1; i++) {
		s1[j] = s0[i];
		j++;
	}
	s1[j] = 0;
	return n;
}

void sfill(cell a, cell n) {
	int	c, i, k;
	char	*s;

	if (!string_p(a)) expect("string-fill!", "string", a);
	if (constant_p(a)) error("string-fill!: immutable", a);
	if (!char_p(n)) expect("sfill", "char", n);
	c = char_value(n);
	k = string_len(a)-1;
	s = string(a);
	for (i=0; i<k; i++) s[i] = c;
}

cell sconc(cell a, cell b) {
	cell	n;
	int	ka, kb;

	if (!string_p(a)) expect("string-append", "string", a);
	if (!string_p(b)) expect("string-append", "string", b);
	ka = string_len(a)-1;
	kb = string_len(b)-1;
	n = make_string("", ka+kb);
	memcpy(string(n), string(a), ka);
	memcpy(&string(n)[ka], string(b), kb+1);
	return n;
}

cell makevec(cell z, cell a) {
	cell	n;
	int	i, k;
	cell	*v;

	k = integer_value("make-vector", z);
	n = make_vector(k);
	v = vector(n);
	for (i=0; i<k; i++) v[i] = a;
	return n;
}

cell vref(cell s, cell n) {
	int	i;

	if (!vector_p(s)) expect("vector-ref", "vector", s);
	i = integer_value("vector-ref", n);
	if (i < 0 || i >= vector_len(s))
		error("vextor-ref: index out of range", n);
	return vector(s)[i];
}

cell vconc(cell a, cell b) {
	cell	n, *va, *vb, *vn;
	int	ka, kb, i;

	if (!vector_p(a)) expect("vector-append", "vector", a);
	if (!vector_p(b)) expect("vector-append", "vector", b);
	ka = vector_len(a);
	kb = vector_len(b);
	n = make_vector(ka+kb);
	va = vector(a);
	vb = vector(b);
	vn = vector(n);
	for (i=0; i<ka; i++) vn[i] = va[i];
	for (i=0; i<kb; i++) vn[i+ka] = vb[i];
	return n;
}

cell vcopy(cell v, cell n0, cell nn, cell fill) {
	cell	n, *ov, *nv;
	int	k0, kn, k;
	int	i, j;

	k = vector_len(v);
	k0 = 0;
	kn = k;
	if (n0 != UNDEFINED) k0 = integer_value("vector-copy", n0);
	if (nn != UNDEFINED) kn = integer_value("vector-copy", nn);
	if (k0 > kn) error("vector-copy: bad range", UNDEFINED);
	n = make_vector(kn-k0);
	nv = vector(n);
	ov = vector(v);
	for (j = 0, i = k0; i < kn; i++, j++)
		nv[j] = i>=k? fill: ov[i];
	return n;
}

void vfill(cell a, cell n) {
	int	i, k;
	cell	*v;

	if (!vector_p(a)) expect("vector-fill!", "vector", a);
	if (constant_p(a)) error("vector-fill!: immutable", a);
	k = vector_len(a);
	v = vector(a);
	for (i=0; i<k; i++) v[i] = n;
}

void vset(cell s, cell n, cell r) {
	int	i;

	if (!vector_p(s)) expect("vector-set!", "vector", s);
	if (constant_p(s)) error("vector-set!: immutable", s);
	i = integer_value("vector-set!", n);
	if (i < 0 || i >= vector_len(s))
		error("vector-set!: index out of range", n);
	vector(s)[i] = r;
}

cell openfile(cell x, int mode) {
	int	p = 0; /*LINT*/

	switch (mode) {
	case 0:
		p = open_input_port(string(x));
		break;
	case 1:
		p = open_output_port(string(x), 0);
		break;
	case 2:
		p = open_output_port(string(x), 1);
		break;
	}
	if (p < 0) {
		if (0 == mode)
			error("open-input-file: cannot open", x);
		else if (1 == mode)
			error("open-output-file: cannot open", x);
		else
			error("open-append-file: cannot open", x);
	}
	return make_port(p, 0 == mode? T_INPUT_PORT: T_OUTPUT_PORT);
}

cell readchar(cell p, int rej) {
	int	pp, c;

	pp = input_port();
	if (p != pp) set_input_port(p);
	c = readc();
	if (rej) rejectc(c);
	if (p != pp) set_input_port(pp);
	if (EOF == c) return END_OF_FILE;
	return make_char(c);
}

cell read_obj(cell p, int rej) {
	int	pp;
	cell	n;

	pp = input_port();
	if (p != pp) set_input_port(p);
	n = xread();
	if (p != pp) set_input_port(pp);
	return n;
}

void write_obj(cell x, int p, int disp) {
	int	pp, odisp;

	pp = output_port();
	if (p != pp) set_output_port(p);
	odisp = Displaying;
	Displaying = disp;
	print_form(x);
	Displaying = odisp;
	if (p != pp) set_output_port(pp);
}

void writechar(int c, cell p) {
	int	pp;

	pp = output_port();
	if (p != pp) set_output_port(p);
	writec(c);
	if (p != pp) set_output_port(pp);
}

#define whitespc(c) \
	(' ' == (c) || \
	 '\t' == (c) || \
	 '\n' == (c) || \
	 '\r' == (c) || \
	 '\f' == (c))

void dump_image_file(cell s) {
	char	*rc;

	rc = dump_image(string(s), S9magic);
	if (rc != NULL) {
		remove(string(s));
		error(rc, s);
	}
	setbind(S_image_file, s);
}

void loadfile(char *s) {
	int	ldport, rdport;
	cell	x, ld;
	cell	oline;

	ldport = open_input_port(s);
	if (ldport < 0)
		error("load: cannot open file",
			make_string(s, strlen(s)));
	lock_port(ldport);
	rdport = input_port();
	ld = getbind(S_loading);
	setbind(S_loading, TRUE);
	oline = Line_no;
	Line_no = 1;
	save(make_string(Srcfile, strlen(Srcfile)));
	strncpy(Srcfile, s, TOKEN_LENGTH);
	Srcfile[TOKEN_LENGTH] = 0;
	for (;;) {
		set_input_port(ldport);
		x = xread();
		set_input_port(rdport);
		if (END_OF_FILE == x) break;
		eval(x);
	}
	strcpy(Srcfile, string(unsave(1)));
	Line_no = oline;
	setbind(S_loading, ld);
	close_port(ldport);
}

void load(cell x) {
	char	path[TOKEN_LENGTH+1];

	if (!string_p(x)) expect("load", "string", x);
	if (string_len(x) > TOKEN_LENGTH)
		error("load: path too long", x);
	strcpy(path, string(x));
	loadfile(path);
}

cell stats(cell x) {
	counter	*ncs, *ccs, *vcs, *gcs;
	cell	n;

	gcv();
	Stats = 1;
	x = eval(x);
	Stats = 0;
	save(x);
	get_counters(&ncs, &ccs, &vcs, &gcs);
	n = cons(read_counter(gcs), NIL);
	n = cons(read_counter(vcs), n);
	n = cons(read_counter(ccs), n);
	n = cons(read_counter(ncs), n);
	n = cons(x, n);
	unsave(1);
	return n;
}

cell getenvvar(char *s) {
	char	*p;

	p = getenv(s);
	if (NULL == p) return FALSE;
	return make_string(p, strlen(p));
}

cell cvt_bytecode(cell x) {
	cell	b, *v;
	int	i, k;

	k = vector_len(x);
	b = subvector(x, 0, k);
	v = vector(b);
	for (i=0; i<k; i++) {
		if (fix_p(v[i]))
			car(v[i]) = T_INTEGER;
	}
	return b;
}

/* Main interpreter loop */

void reset_tty(void);

void run(cell x) {
	Prog = x;
	if (setjmp(Error_tag) != 0) {
		Ip = throw(Error_handler, getbind(S_error_value));
		if (Ip < 0) longjmp(Restart, 1);
	}
	for (Running = 1; Running;) switch (ins()) {
	case OP_APPLIS:
		Ip = applis(0);
		break;
	case OP_TAIL_APPLIS:
		Ip = applis(1);
		break;
	case OP_TAIL_APPLY:
		Ip = apply(1);
		break;
	case OP_APPLY:
		Ip = apply(0);
		break;
	case OP_QUOTE:
		Acc = op1();
		skip(2);
		break;
	case OP_ARG:
		Acc = argref(op1());
		skip(2);
		break;
	case OP_REF:
		Acc = boxref(envbox(op1()));
		if (UNDEFINED == Acc)
			error("undefined symbol", op2());
		if (Tp >= MAX_REF_TRACE) Tp = 0;
		Trace[Tp++] = op2();
		skip(3);
		break;
	case OP_POP:
		Acc = stackref(Sp);
		Sp--;
		skip(1);
		break;
	case OP_PUSH:
		push(cons(Acc, NIL));
		skip(1);
		break;
	case OP_PUSH_VAL:
		push(op1());
		skip(2);
		break;
	case OP_JMP:
		Ip = fixval(op1());
		break;
	case OP_JMP_FALSE:
		if (FALSE == Acc)
			Ip = fixval(op1());
		else
			skip(2);
		break;
	case OP_JMP_TRUE:
		if (FALSE == Acc)
			skip(2);
		else
			Ip = fixval(op1());
		break;
	case OP_HALT:
		return;
	case OP_MAKE_ENV:
		Acc = make_vector(fixval(op1()));
		skip(2);
		break;
	case OP_PROP_ENV:
		Acc = Ep;
		skip(1);
		break;
	case OP_COPY_ARG:
		vector(Acc)[fixval(op2())] = argbox(op1());
		skip(3);
		break;
	case OP_COPY_REF:
		vector(Acc)[fixval(op2())] = envbox(op1());
		skip(3);
		break;
	case OP_CLOSURE:
		Acc = closure(op1(), Acc);
		skip(2);
		break;
	case OP_ENTER:
		if (fixval(stackref(Sp-2)) != fixval(op1()))
			error("wrong number of arguments", UNDEFINED);
		push(mkfix(Fp));
		Fp = Sp-4;
		skip(2);
		break;
	case OP_ENTER_COLL:
		entcol(fixval(op1()));
		skip(2);
		break;
	case OP_RETURN:
		Ip = ret();
		break;
	case OP_SET_ARG:
		boxset(argbox(op1()), Acc);
		Acc = UNSPECIFIC;
		skip(2);
		break;
	case OP_SET_REF:
		boxset(envbox(op1()), Acc);
		Acc = UNSPECIFIC;
		skip(2);
		break;
	case OP_DEF_MACRO:
		newmacro(op1(), Acc);
		Acc = UNSPECIFIC;
		skip(2);
		break;
	case OP_COMMAND_LINE:
		Acc = Argv;
		skip(1);
		break;
	case OP_CURRENT_ERROR_PORT:
		Acc = make_port(error_port(), T_OUTPUT_PORT);
		skip(1);
		break;
	case OP_CURRENT_INPUT_PORT:
		Acc = make_port(input_port(), T_INPUT_PORT);
		skip(1);
		break;
	case OP_CURRENT_OUTPUT_PORT:
		Acc = make_port(output_port(), T_OUTPUT_PORT);
		skip(1);
		break;
	case OP_GENSYM:
		Acc = gensym();
		skip(1);
		break;
	case OP_QUIT:
		reset_tty();
		bye(0);
		skip(1);
		break;
	case OP_SYMBOLS:
		Acc = carof(Glob);
		skip(1);
		break;
	case OP_ABS:
		if (!number_p(Acc)) expect("abs", "number", Acc);
		Acc = real_abs(Acc);
		skip(1);
		break;
	case OP_BOOLEAN_P:
		Acc = (TRUE == Acc || FALSE == Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_CHAR_ALPHABETIC_P:
		if (!char_p(Acc)) expect("char-alphabetic?", "char", Acc);
		Acc = isalpha(char_value(Acc))? TRUE: FALSE;
		skip(1);
		break;
	case OP_CAR:
		if (!pair_p(Acc)) expect("car", "pair", Acc);
		Acc = car(Acc);
		skip(1);
		break;
	case OP_CATCH:
		push(box(catch()));
		push(mkfix(1));
		skip(1);
		break;
	case OP_CATCH_TAG_P:
		Acc = catch_tag_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_CDR:
		if (!pair_p(Acc)) expect("cdr", "pair", Acc);
		Acc = cdr(Acc);
		skip(1);
		break;
	case OP_CAAR:
		if (!pair_p(Acc) || !pair_p(car(Acc)))
			expect("caar", "nested pair", Acc);
		Acc = caar(Acc);
		skip(1);
		break;
	case OP_CADR:
		if (!pair_p(Acc) || !pair_p(cdr(Acc)))
			expect("cadr", "nested pair", Acc);
		Acc = cadr(Acc);
		skip(1);
		break;
	case OP_CDAR:
		if (!pair_p(Acc) || !pair_p(car(Acc)))
			expect("cdar", "nested pair", Acc);
		Acc = cdar(Acc);
		skip(1);
		break;
	case OP_CDDR:
		if (!pair_p(Acc) || !pair_p(cdr(Acc)))
			expect("cddr", "nested pair", Acc);
		Acc = cddr(Acc);
		skip(1);
		break;
	case OP_CAAAR:
		Acc = cxr("aaa", Acc);
		skip(1);
		break;
	case OP_CAADR:
		Acc = cxr("daa", Acc);
		skip(1);
		break;
	case OP_CADAR:
		Acc = cxr("ada", Acc);
		skip(1);
		break;
	case OP_CADDR:
		Acc = cxr("dda", Acc);
		skip(1);
		break;
	case OP_CDAAR:
		Acc = cxr("aad", Acc);
		skip(1);
		break;
	case OP_CDADR:
		Acc = cxr("dad", Acc);
		skip(1);
		break;
	case OP_CDDAR:
		Acc = cxr("add", Acc);
		skip(1);
		break;
	case OP_CDDDR:
		Acc = cxr("ddd", Acc);
		skip(1);
		break;
	case OP_CAAAAR:
		Acc = cxr("aaaa", Acc);
		skip(1);
		break;
	case OP_CAAADR:
		Acc = cxr("daaa", Acc);
		skip(1);
		break;
	case OP_CAADAR:
		Acc = cxr("adaa", Acc);
		skip(1);
		break;
	case OP_CAADDR:
		Acc = cxr("ddaa", Acc);
		skip(1);
		break;
	case OP_CADAAR:
		Acc = cxr("aada", Acc);
		skip(1);
		break;
	case OP_CADADR:
		Acc = cxr("dada", Acc);
		skip(1);
		break;
	case OP_CADDAR:
		Acc = cxr("adda", Acc);
		skip(1);
		break;
	case OP_CADDDR:
		Acc = cxr("ddda", Acc);
		skip(1);
		break;
	case OP_CDAAAR:
		Acc = cxr("aaad", Acc);
		skip(1);
		break;
	case OP_CDAADR:
		Acc = cxr("daad", Acc);
		skip(1);
		break;
	case OP_CDADAR:
		Acc = cxr("adad", Acc);
		skip(1);
		break;
	case OP_CDADDR:
		Acc = cxr("ddad", Acc);
		skip(1);
		break;
	case OP_CDDAAR:
		Acc = cxr("aadd", Acc);
		skip(1);
		break;
	case OP_CDDADR:
		Acc = cxr("dadd", Acc);
		skip(1);
		break;
	case OP_CDDDAR:
		Acc = cxr("addd", Acc);
		skip(1);
		break;
	case OP_CDDDDR:
		Acc = cxr("dddd", Acc);
		skip(1);
		break;
	case OP_CALL_CC:
		push(box(capture_cont()));
		push(mkfix(1));
		skip(1);
		break;
	case OP_ERROR:
		if (!string_p(Acc)) expect("error", "string", Acc);
		error(string(Acc), UNDEFINED);
		skip(1);
		break;
	case OP_ERROR2:
		if (!string_p(Acc)) expect("error", "string", Acc);
		error(string(Acc), arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CEILING:
		if (!number_p(Acc)) expect("ceiling", "number", Acc);
		Acc = real_ceil(Acc);
		skip(1);
		break;
	case OP_INTEGER_TO_CHAR:
		Acc = integer_value("integer->char", Acc);
		if (Acc < 0 || Acc > 126)
			error("integer->char: value out of range", Acc);
		Acc = make_char(Acc);
		skip(1);
		break;
	case OP_CHAR_P:
		Acc = char_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_CHAR_TO_INTEGER:
		if (!char_p(Acc)) expect("char->integer", "char", Acc);
		Acc = make_integer(char_value(Acc));
		skip(1);
		break;
	case OP_CLOSE_INPUT_PORT:
		if (!input_port_p(Acc))
			expect("close-input-port", "input port", Acc);
		close_port(port_no(Acc));
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_CLOSE_OUTPUT_PORT:
		if (!output_port_p(Acc))
			expect("close-output-port", "output port", Acc);
		close_port(port_no(Acc));
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_DELETE_FILE:
		if (!string_p(Acc)) expect("delete-file", "string", Acc);
		if (remove(string(Acc)) < 0)
			error("delete-file: cannot delete", Acc);
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_CHAR_DOWNCASE:
		if (!char_p(Acc)) expect("char-downcase", "char", Acc);
		Acc = make_char(tolower(char_value(Acc)));
		skip(1);
		break;
	case OP_DUMP_IMAGE:
		if (!string_p(Acc)) expect("dump-image", "string", Acc);
		dump_image_file(Acc);
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_ENVIRONMENT_VARIABLE:
		if (!string_p(Acc))
			expect("environment-variable", "string", Acc);
		Acc = getenvvar(string(Acc));
		skip(1);
		break;
	case OP_EOF_OBJECT_P:
		Acc = (END_OF_FILE == Acc? TRUE: FALSE);
		skip(1);
		break;
	case OP_EVAL:
		Acc = eval(Acc);
		skip(1);
		break;
	case OP_EVEN_P:
		Acc = integer_argument("even?", Acc);
		Acc = bignum_even_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_EXACT_TO_INEXACT:
		if (!number_p(Acc)) expect("exact->inexact", "number", Acc);
		Acc = exact_to_inexact(Acc);
		skip(1);
		break;
	case OP_EXACT_P:
		if (!number_p(Acc)) expect("exact?", "number", Acc);
		Acc = integer_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_EXPONENT:
		if (!number_p(Acc)) expect("exponent", "number", Acc);
		Acc = make_integer(real_exponent(Acc));
		skip(1);
		break;
	case OP_FLOOR:
		if (!number_p(Acc)) expect("floor", "number", Acc);
		Acc = real_floor(Acc);
		skip(1);
		break;
	case OP_FILE_EXISTS_P:
		if (!string_p(Acc)) expect("file-exists?", "string", Acc);
		Acc = exists_p(string(Acc));
		skip(1);
		break;
	case OP_FIX_EXACTNESS:
		if (TRUE == vector(Rts)[Sp--])
			Acc = exact_to_inexact(Acc);
		skip(1);
		break;
	case OP_INEXACT_TO_EXACT:
		if (!number_p(Acc)) expect("inexact->exact", "number", Acc);
		Acc = inexact_to_exact(Acc);
		skip(1);
		break;
	case OP_INEXACT_P:
		if (!number_p(Acc)) expect("inexact?", "number", Acc);
		Acc = real_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_INTEGER_P:
		Acc = real_integer_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_MACRO_EXPAND:
	case OP_MACRO_EXPAND_1:
		Acc = expand(Acc, OP_MACRO_EXPAND == ins());
		skip(1);
		break;
	case OP_MANTISSA:
		if (!number_p(Acc)) expect("exponent", "number", Acc);
		Acc = real_mantissa(Acc);
		skip(1);
		break;
	case OP_LOAD:
		load(Acc);
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_PROCEDURE_P:
		Acc = function_p(Acc) ||
		      continuation_p(Acc) ||
		      primitive_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_INPUT_PORT_P:
		Acc = input_port_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_LIST_TO_STRING:
		if (!list_p(Acc)) expect("list->string", "list", Acc);
		Acc = list_to_string(Acc);
		skip(1);
		break;
	case OP_LIST_TO_VECTOR:
		if (!list_p(Acc)) expect("list->vector", "list", Acc);
		Acc = list_to_vector(Acc, "list->vector: improper list", 0);
		skip(1);
		break;
	case OP_CHAR_LOWER_CASE_P:
		if (!char_p(Acc)) expect("char-lower-case?", "char", Acc);
		Acc = islower(char_value(Acc))? TRUE: FALSE;
		skip(1);
		break;
	case OP_NEGATE:
		if (!number_p(Acc)) expect("-", "number", Acc);
		Acc = real_negate(Acc);
		skip(1);
		break;
	case OP_LENGTH:
		if (!list_p(Acc)) expect("length", "list", Acc);
		Acc = list_length(Acc);
		skip(1);
		break;
	case OP_NOT:
		Acc = (FALSE == Acc? TRUE: FALSE);
		skip(1);
		break;
	case OP_NULL_P:
		Acc = (NIL == Acc? TRUE: FALSE);
		skip(1);
		break;
	case OP_CHAR_NUMERIC_P:
		if (!char_p(Acc)) expect("char-numeric?", "char", Acc);
		Acc = isdigit(char_value(Acc))? TRUE: FALSE;
		skip(1);
		break;
	case OP_NEGATIVE_P:
		if (!number_p(Acc)) expect("negative?", "number", Acc);
		Acc = real_negative_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_ODD_P:
		Acc = integer_argument("odd?", Acc);
		Acc = bignum_even_p(Acc)? FALSE: TRUE;
		skip(1);
		break;
	case OP_OPEN_APPEND_FILE:
		if (!string_p(Acc)) expect("open-append-file", "string", Acc);
		Acc = openfile(Acc, 2);
		skip(1);
		break;
	case OP_OPEN_INPUT_FILE:
		if (!string_p(Acc)) expect("open-input-file", "string", Acc);
		Acc = openfile(Acc, 0);
		skip(1);
		break;
	case OP_OPEN_OUTPUT_FILE:
		if (!string_p(Acc)) expect("open-output-file", "string", Acc);
		Acc = openfile(Acc, 1);
		skip(1);
		break;
	case OP_OUTPUT_PORT_P:
		Acc = output_port_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_PAIR_P:
		Acc = pair_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_POSITIVE_P:
		if (!number_p(Acc)) expect("positive?", "number", Acc);
		Acc = real_positive_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_PEEK_CHAR:
		if (!input_port_p(Acc)) expect("peek-char", "input port", Acc);
		Acc = readchar(port_no(Acc), 1);
		skip(1);
		break;
	case OP_READ:
		if (!input_port_p(Acc)) expect("read", "input port", Acc);
		Acc = read_obj(port_no(Acc), 0);
		skip(1);
		break;
	case OP_READ_CHAR:
		if (!input_port_p(Acc)) expect("read-char", "input port", Acc);
		Acc = readchar(port_no(Acc), 0);
		skip(1);
		break;
	case OP_REAL_P:
		Acc = number_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_REVERSE:
		if (!list_p(Acc)) expect("reverse", "list", Acc);
		Acc = reverse(Acc);
		skip(1);
		break;
	case OP_REVERSE_B:
		if (!list_p(Acc)) expect("reverse!", "list", Acc);
		Acc = nreverse(Acc);
		skip(1);
		break;
	case OP_S9_BYTECODE:
		if (!function_p(Acc)) expect("s9:bytecode", "procedure", Acc);
		Acc = cvt_bytecode(closure_prog(Acc));
		skip(1);
		break;
	case OP_STATS:
		Acc = stats(Acc);
		skip(1);
		break;
	case OP_SYSTEM_COMMAND:
		if (!string_p(Acc)) expect("system-command", "string", Acc);
		Acc = make_integer(system(string(Acc)) >> 8);
		skip(1);
		break;
	case OP_TRUNCATE:
		if (!number_p(Acc)) expect("truncate", "number", Acc);
		Acc = real_trunc(Acc);
		skip(1);
		break;
	case OP_ZERO_P:
		if (!number_p(Acc)) expect("zero?", "number", Acc);
		Acc = real_zero_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_APPEND:
		if (!list_p(Acc)) expect("append", "list", Acc);
		Acc = append(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_APPEND:
		Acc = sconc(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_VECTOR_APPEND:
		Acc = vconc(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_SET_INPUT_PORT_B:
		if (!input_port_p(Acc))
			expect("set-input-port!", "input port", Acc);
		set_input_port(port_no(Acc));
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_SET_OUTPUT_PORT_B:
		if (!output_port_p(Acc))
			expect("set-output-port!", "output port", Acc);
		set_output_port(port_no(Acc));
		Acc = UNSPECIFIC;
		skip(1);
		break;
	case OP_STRING_COPY:
		if (!string_p(Acc)) expect("string-copy", "string", Acc);
		Acc = copy_string(Acc);
		skip(1);
		break;
	case OP_STRING_LENGTH:
		if (!string_p(Acc)) expect("string-length", "string", Acc);
		Acc = make_integer(string_len(Acc)-1);
		skip(1);
		break;
	case OP_SYMBOL_P:
		Acc = symbol_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_STRING_TO_SYMBOL:
		if (!string_p(Acc)) expect("string->symbol", "string", Acc);
		Acc = string_to_symbol(Acc);
		skip(1);
		break;
	case OP_SYMBOL_TO_STRING:
		if (!symbol_p(Acc)) expect("symbol->string", "symbol", Acc);
		Acc = symbol_to_string(Acc);
		skip(1);
		break;
	case OP_STRING_P:
		Acc = string_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_STRING_TO_LIST:
		if (!string_p(Acc)) expect("string->list", "string", Acc);
		Acc = string_to_list(Acc);
		skip(1);
		break;
	case OP_CHAR_UPCASE:
		if (!char_p(Acc)) expect("char-upcase", "char", Acc);
		Acc = make_char(toupper(char_value(Acc)));
		skip(1);
		break;
	case OP_CHAR_UPPER_CASE_P:
		if (!char_p(Acc)) expect("char-upper-case?", "char", Acc);
		Acc = isupper(char_value(Acc))? TRUE: FALSE;
		skip(1);
		break;
	case OP_VECTOR_TO_LIST:
		if (!vector_p(Acc)) expect("vector->list", "vector", Acc);
		Acc = vector_to_list(Acc);
		skip(1);
		break;
	case OP_VECTOR_P:
		Acc = vector_p(Acc)? TRUE: FALSE;
		skip(1);
		break;
	case OP_VECTOR_LENGTH:
		if (!vector_p(Acc)) expect("vector-length", "vector", Acc);
		Acc = make_integer(vector_len(Acc));
		skip(1);
		break;
	case OP_CHAR_WHITESPACE_P:
		if (!char_p(Acc)) expect("char-whitespace?", "char", Acc);
		Acc = whitespc(char_value(Acc))? TRUE: FALSE;
		skip(1);
		break;
	case OP_CHAR_LESS_P:
		cless(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_LTEQ_P:
		clteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_EQUAL_P:
		cequal(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_GRTR_P:
		cgrtr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_GTEQ_P:
		cgteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_CI_LESS_P:
		ciless(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_CI_LTEQ_P:
		cilteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_CI_EQUAL_P:
		ciequal(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_CI_GRTR_P:
		cigrtr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CHAR_CI_GTEQ_P:
		cigteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_ASSQ:
		if (!list_p(arg(0))) expect("assq", "alist", arg(0));
		Acc = assqv("assq", 0, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_ASSV:
		if (!list_p(arg(0))) expect("assv", "alist", arg(0));
		Acc = assqv("assv", 1, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_CONS:
		Acc = cons(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_EQV_P:
		Acc = eqv_p(Acc, arg(0))? TRUE: FALSE;
		clear(1);
		skip(1);
		break;
	case OP_EQ_P:
		Acc = (Acc == arg(0))? TRUE: FALSE;
		clear(1);
		skip(1);
		break;
	case OP_EQUAL:
		equal(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_GRTR:
		grtr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_GTEQ:
		gteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_LESS:
		less(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_LTEQ:
		lteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_LIST_REF:
		if (!list_p(Acc)) expect("list-ref", "list", Acc);
		Acc = nth("list-ref", 1, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_LIST_TAIL:
		if (!list_p(Acc)) expect("list-tail", "list", Acc);
		Acc = nth("list-tail", 0, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_MEMQ:
		if (!list_p(arg(0))) expect("memq", "list", arg(0));
		Acc = memqv("memq", 0, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_MEMV:
		if (!list_p(arg(0))) expect("memv", "list", arg(0));
		Acc = memqv("memv", 1, Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_THROW:
		Ip = throw(Acc, arg(0));
		break;
	case OP_MAX:
		if (real_p(Acc) || real_p(arg(0))) stackset(Sp-1, TRUE);
		Acc = real_less_p(arg(0), Acc)? Acc: arg(0);
		clear(1);
		skip(1);
		break;
	case OP_MIN:
		if (real_p(Acc) || real_p(arg(0))) stackset(Sp-1, TRUE);
		Acc = real_less_p(Acc, arg(0))? Acc: arg(0);
		clear(1);
		skip(1);
		break;
	case OP_MINUS:
		Acc = xsub(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_MAKE_STRING:
		Acc = makestr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_MAKE_VECTOR:
		Acc = makevec(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_PLUS:
		Acc = add(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_BIT_OP:
		Acc = bit_op(Acc, arg(0), arg(1));
		clear(2);
		skip(1);
		break;
	case OP_WRITE:
		if (!output_port_p(arg(0)))
			expect("write", "output port", arg(0));
		write_obj(Acc, port_no(arg(0)), 0);
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_DISPLAY:
		if (!output_port_p(arg(0)))
			expect("display", "output port", arg(0));
		write_obj(Acc, port_no(arg(0)), 1);
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_DIVIDE:
		Acc = xdiv(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_QUOTIENT:
		Acc = intdiv(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_REMAINDER:
		Acc = intrem(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_SET_CAR_B:
		if (!pair_p(Acc)) expect("set-car!", "pair", Acc);
		if (constant_p(Acc)) error("set-car!: immutable", Acc);
		car(Acc) = arg(0);
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_SET_CDR_B:
		if (!pair_p(Acc)) expect("set-cdr!", "pair", Acc);
		if (constant_p(Acc)) error("set-cdr!: immutable", Acc);
		cdr(Acc) = arg(0);
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_STRING_LESS_P:
		sless(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_LTEQ_P:
		slteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_EQUAL_P:
		sequal(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_GRTR_P:
		sgrtr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_GTEQ_P:
		sgteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SI_LESS_P:
		siless(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SI_LTEQ_P:
		silteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SI_EQUAL_P:
		siequal(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SI_GRTR_P:
		sigrtr(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SI_GTEQ_P:
		sigteq(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_FILL_B:
		sfill(Acc, arg(0));
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_STRING_REF:
		Acc = sref(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_STRING_SET_B:
		sset(Acc, arg(0), arg(1));
		Acc = UNSPECIFIC;
		clear(2);
		skip(1);
		break;
	case OP_SUBSTRING:
		Acc = substring(Acc, arg(0), arg(1));
		clear(2);
		skip(1);
		break;
	case OP_TIMES:
		Acc = mul(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_VECTOR_FILL_B:
		vfill(Acc, arg(0));
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_VECTOR_REF:
		Acc = vref(Acc, arg(0));
		clear(1);
		skip(1);
		break;
	case OP_VECTOR_SET_B:
		vset(Acc, arg(0), arg(1));
		Acc = UNSPECIFIC;
		clear(2);
		skip(1);
		break;
	case OP_WRITE_CHAR:
		if (!char_p(Acc)) expect("write-char", "char", Acc);
		if (!output_port_p(arg(0)))
			expect("write-char", "output port", arg(0));
		writechar(char_value(Acc), port_no(arg(0)));
		Acc = UNSPECIFIC;
		clear(1);
		skip(1);
		break;
	case OP_VECTOR_COPY:
		if (!vector_p(Acc)) expect("vector-copy", "vector", Acc);
		Acc = vcopy(Acc, arg(0), arg(1), arg(2));
		clear(3);
		skip(1);
		break;
	default:
		error("illegal instruction", make_integer(ins()));
		return;
	}
	error("interrupted", UNDEFINED);
}

/*
 * Evaluator
 */

cell interpret(cell x) {
	cell	n, *v;
	int	i;

	Ip = 0;
	/*Sp = -1;
	Fp = -1;*/
	E0 = make_vector(length(Glob));
	i = 0;
	v = vector(E0);
	for (n = Glob; n != NIL; n = cdr(n)) {
		v[i] = cdar(n);
		i++;
	}
	Ep = E0;
	if (Stats) {
		run_stats(1);
		cons_stats(1);
	}
	run(x);
	if (Stats) {
		cons_stats(0);
		run_stats(0);
	}
	v = vector(E0);
	i = 0;
	for (n = Glob; n != NIL; n = cdr(n)) {
		cdar(n) = v[i];
		i++;
	}
	return Acc;
}

void begin_rec(void) {
	save(Prog);
	save(Ep);
	save(mkfix(Ip));
	save(mkfix(Sp));
	save(mkfix(Fp));
}

void end_rec(void) {
	Fp = fixval(unsave(1));
	Sp = fixval(unsave(1));
	Ip = fixval(unsave(1));
	Ep = unsave(1);
	Prog = unsave(1);
}

cell eval(cell x) {
	Tmp = x;
	begin_rec();
	save(x);
	Tmp = NIL;
	x = expand(x, 1);
	car(Stack) = x;
	syncheck(x, 1);
	x = clsconv(x);
	car(Stack) = x;
	x = compile(x);
	car(Stack) = x;
	x = interpret(x);
	unsave(1);
	end_rec();
	return x;
}

/*
 * REPL
 */

#ifdef unix
 void keyboard_interrupt(int sig) {
	reset_std_ports();
	s9_abort();
	Running = 0;
	Expand_level = -1;
	Intr = 1;
 }

 void keyboard_quit(int sig) {
	fatal("received QUIT signal, exiting");
 }
#endif /* unix */

#ifdef plan9
 void keyboard_interrupt(void *dummy, char *note) {
	if (strstr(note, "interrupt") == NULL)
		noted(NDFLT);
	reset_std_ports();
	s9_abort();
	Running = 0;
	Expand_level = -1;
	Intr = 1;
	noted(NCONT);
 }
#endif /* plan9 */

void mem_error(int src) {
	if (1 == src)
		error("node limit reached", UNDEFINED);
	else    
		error("vector limit reached", UNDEFINED);
}

void reset_tty(void) {
#ifdef CURSES_RESET
	cell pp_curs_endwin(cell x);
	pp_curs_endwin(NIL);
#endif
}

void repl(void) {
	cell	x;

	setjmp(Restart);
	if (!O_quiet) handle_sigint();
	handle_sigquit();
	mem_error_handler(mem_error);
	for (;;) {
		Running = 0;
		Expand_level = 0;
		reset_tty();
		s9_reset();
		reset_std_ports();
		clear_trace();
		Stack = NIL;
		init_rts();
		if (!O_quiet) {
			prints("> ");
			flush();
		}
		Intr = 0;
		x = xread();
		if (END_OF_FILE == x && 0 == Intr) break;
		x = eval(x);
		if (x != UNSPECIFIC) {
			setbind(S_starstar, x);
			print_form(x);
			nl();
		}
	}
	if (!O_quiet) nl();
}

/*
 * Command line interface
 */

void evalstr(char *s, int echo) {
	cell	x;

	clear_trace();
	x = xsread(s);
	if (UNDEFINED == x) return;
	x = eval(x);
	if (echo) {
		print_form(x);
		nl();
	}
}

void usage(void) {
	prints(
	"Usage: s9 [-i file | -] [-hv?] [-gqu] [-e expr] [-d file]\n"
	"          [-k cells] [-l file] [-n nodes] [-r expr]\n"
	"          [-- argument ... | [-f] file argument ...]\n");
}

void longusage(void) {
	char	b[100];
	cell	x;

	nl();
	prints("Scheme 9 from Empty Space by Nils M Holm, ");
	prints(RELEASE_DATE);
	if (PATCHLEVEL) {
		prints(" pl");
		writec(PATCHLEVEL+'0');
	}
	nl();
	prints("This program is in the public domain");
	nl();
	x = getbind(S_extensions);
	if (pair_p(x)) {
		prints("Extensions:");
		for (; x != NIL; x = cdr(x)) {
			writec(' ');
			print_form(car(x));
		}
		nl();
	}
	nl();
	usage();
	prints(	"\n"
		"-h         help (also -v, -?)\n"
		"-d file    dump image to file, then exit\n"
		"-g         print garbage collector messages (-gg = more)\n"
		"-e expr    evaluate expression, print value, no REPL\n"
		"-i file    load image from file (default: ");
	prints(IMAGE_FILE);
	prints(")\n");
	prints(	"-i -       compile initial image from sources (s9.scm)\n"
		"-k n[m]    set vector limit to nK (or nM) cells (");
	prints(ntoa(b, S9_VECTOR_LIMIT, 0));
	prints(	"K)\n"
		"-l file    load program from file\n"
		"-n n[m]    set node limit to nK (or nM) nodes (");
	prints(ntoa(b, S9_NODE_LIMIT, 0));
	prints(	"K)\n"
		"-q         quiet: no banner, no prompt, exit on errors\n"
		"-r expr    like -e, but don't print value (run for effect)\n"
		"-u         use unlimited node and vector memory\n"
		"file args  run program file with arguments, then exit\n"
		"-- args    bind remaining arguments to (command-line)\n"
		"\n");
	bye(0);
}

long get_size_k(char *s) {
	int	c;
	long	n;

	c = s[strlen(s)-1];
	n = asctol(s);
	if ('M' == c || 'm' == c)
		return n * 1024L;
	else if (!isdigit(c)) {
		usage();
		bye(1);
	}
	return n;
}

char *cmdarg(char *s) {
	if (NULL == s) {
		usage();
		bye(1);
	}
	return s;
}

int main(int argc, char **argv) {
	int	i, j, k, g, t, loop;
	char	*s, *dump;
	cell	libs, exprs, n;

	if (setjmp(Restart) != 0) bye(1);
	init();
	i = 1;
	if (argc > 2 && strcmp(argv[1], "-i") == 0) {
		load_initial_image(argv[2]);
		i = 3;
	}
	else {
		load_initial_image(IMAGE_FILE);
	}
	setbind(S_library_path, make_library_path());
	save(libs = NIL);
	save(exprs = NIL);
	dump = NULL;
	loop = 1;
	g = 0;
	for (; i<argc; i++) {
		if (argv[i][0] != '-') break;
		if ('-' == argv[i][1]) break;
		if ('f' == argv[i][1]) {
			i++;
			break;
		}
		k = strlen(argv[i]);
		for (j=1; j<k; j++) {
			switch (argv[i][j]) {
			case '?':
			case 'h':
			case 'v':
				longusage();
				break;
			case 'd':
				i++;
				dump = cmdarg(argv[i]);
				j = strlen(argv[i]);
				break;
			case 'r':
			case 'e':
				loop = 0;
				t = 'e' == argv[i][j];
				i++;
				s = cmdarg(argv[i]);
				n = cons(t? TRUE: FALSE,
					make_string(s, strlen(s)));
				exprs = cons(n, exprs);
				car(Stack) = exprs;
				j = strlen(argv[i]);
				break;
			case 'g':
				g++;
				break;
			case 'k':
				i++;
				s = cmdarg(argv[i]);
				set_vector_limit(get_size_k(s));
				j = strlen(argv[i]);
				break;
			case 'l':
				i++;
				s = cmdarg(argv[i]);
				libs = cons(make_string(s, strlen(s)), libs);
				cadr(Stack) = libs;
				j = strlen(argv[i]);
				break;
			case 'n':
				i++;
				s = cmdarg(argv[i]);
				set_node_limit(get_size_k(s));
				j = strlen(argv[i]);
				break;
			case 'q':
				O_quiet = 1;
				break;
			case 'u':
				set_node_limit(0);
				set_vector_limit(0);
				break;
			default:
				usage();
				bye(1);
			}
		}
	}
	gc_verbosity(g);
	init_extensions();
	if (libs != NIL) {
		if (setjmp(Restart) != 0) bye(1);
		libs = nreverse(libs);
		cadr(Stack) = libs;
		while (libs != NIL) {
			loadfile(string(car(libs)));
			libs = cdr(libs);
		}
	}
	if (exprs != NIL) {
		if (setjmp(Restart) != 0) bye(1);
		exprs = nreverse(exprs);
		car(Stack) = exprs;
		while (exprs != NIL) {
			evalstr(string(cdar(exprs)), TRUE == caar(exprs));
			exprs = cdr(exprs);
		}
	}
	unsave(2);
	Argv = NULL == argv[i]? NIL: argv_to_list(&argv[i+1]);
	setbind(S_arguments, Argv);
	if (dump != NULL) {
		dump_image(dump, S9magic);
		bye(0);
	}
	if (argv[i] != NULL && strcmp(argv[i], "--")) {
		if (setjmp(Restart) != 0) bye(1);
		loadfile(argv[i]);
		bye(0);
	}
	if (loop) {
		if (!O_quiet) {
			prints("Scheme 9 from Empty Space (Reimagined)");
			nl();
		}
		repl();
	}
	bye(0);
	return 0;
}
