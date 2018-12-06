/*
 * S9core Toolkit, Mk IVc
 * By Nils M Holm, 2007-2018
 * In the public domain
 *
 * Under jurisdictions without a public domain, the CC0 applies:
 * https://creativecommons.org/publicdomain/zero/1.0/
 */

#define S9_VERSION "20181111"

/*
 * Ugly prelude to deal with some system-dependent stuff.
 */

#ifdef __NetBSD__
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __unix
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __linux
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __GNUC__
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __clang__
 #ifndef unix
  #define unix
 #endif
#endif

#ifndef unix
 #ifndef plan9
  #error "Either 'unix' or 'plan9' must be #defined."
 #endif
#endif

#ifdef _MSC_VER
 #if _MSC_VER > 1200
  #ifndef _CRT_SECURE_NO_DEPRECATE
   #define _CRT_SECURE_NO_DEPRECATE
  #endif
 #endif
 #ifndef _POSIX_
  #define _POSIX_
 #endif
#endif

#ifdef plan9
 #include <u.h>
 #include <libc.h>
 #include <stdio.h>
 #include <ctype.h>
 #include <ape/limits.h>
 #define size_t	uvlong
 #define bye(x)	exits((x)? "error": NULL)
 #define ptrdiff_t vlong
#endif

#ifdef unix
 #include <stdlib.h>
 #include <stddef.h>
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #include <limits.h>
 #define bye(x)	exit((x)? EXIT_FAILURE: EXIT_SUCCESS)
#endif

/*
 * Tunable parameters
 */

/* Default memory limits in K nodes and K cells; 0 = none */
#define S9_NODE_LIMIT	14013
#define S9_VECTOR_LIMIT	14013

/* Initial memory pool sizes */
#define S9_INITIAL_SEGMENT_SIZE	32768

/* Primitive segment size (slots) */
#define S9_PRIM_SEG_SIZE	256

/* Maximum number of open I/O ports */
#define S9_MAX_PORTS		32

/* Use 64-bit cells (don't do this!) */
/* #define S9_BITS_PER_WORD_64 */

/*
 * Non-tunable parameters
 */

/* A cell must be large enough to hold an integer segment;
 * see S9_INT_SEG_LIMIT, below
 */

#ifdef S9_BITS_PER_WORD_64
 #define s9_cell	ptrdiff_t
#else
 #define s9_cell	int
#endif

#ifdef S9_BITS_PER_WORD_64
 #define S9_DIGITS_PER_CELL	18
 #define S9_INT_SEG_LIMIT	1000000000000000000LL
 #define S9_MANTISSA_SEGMENTS	1
#else
 #define S9_DIGITS_PER_CELL	9
 #define S9_INT_SEG_LIMIT	1000000000L
 #define S9_MANTISSA_SEGMENTS	2
#endif

#define S9_MANTISSA_SIZE	(S9_MANTISSA_SEGMENTS * S9_DIGITS_PER_CELL)

/*
 * Node tags
 */

#define S9_ATOM_TAG	0x01	/* Atom, car = type, cdr = next */
#define S9_MARK_TAG	0x02	/* Mark */
#define S9_STATE_TAG	0x04	/* State */
#define S9_VECTOR_TAG	0x08	/* Vector, car = type, cdr = content */
#define S9_PORT_TAG	0x10	/* Atom is an I/O port (with ATOM_TAG) */
#define S9_USED_TAG	0x20	/* Port: in use */
#define S9_LOCK_TAG	0x40	/* Port: locked (do not finalize) */
#define S9_CONST_TAG	0x80	/* Node is immutable */

/*
 * Special objects
 */

#define s9_special_p(x)	((x) < 0)
#define S9_NIL		(-1)
#define S9_TRUE		(-2)
#define S9_FALSE	(-3)
#define S9_END_OF_FILE	(-4)
#define S9_UNDEFINED	(-5)
#define S9_UNSPECIFIC	(-6)
#define S9_VOID		(-7)

/*
 * Type tags
 */

#define S9_T_ANY		(-10)
#define S9_T_BOOLEAN		(-11)
#define S9_T_CHAR		(-12)
#define S9_T_INPUT_PORT		(-13)
#define S9_T_INTEGER		(-14)
#define S9_T_LIST		(-17)
#define S9_T_OUTPUT_PORT	(-15)
#define S9_T_PAIR		(-16)
#define S9_T_PRIMITIVE		(-18)
#define S9_T_FUNCTION		(-19)
#define S9_T_REAL		(-20)
#define S9_T_STRING		(-21)
#define S9_T_SYMBOL		(-22)
#define S9_T_SYNTAX		(-23)
#define S9_T_VECTOR		(-24)
#define S9_T_CONTINUATION	(-25)
#define S9_T_FIXNUM		(-26)
#define S9_T_NONE		(-99)

#define S9_USER_SPECIALS	(-100)

/*
 * Structures
 */

struct S9_counter {
	int	n, n1k, n1m, n1g, n1t;
};

#define s9_counter	struct S9_counter

struct S9_primitive {
	char	*name;
	s9_cell	(*handler)(void);
	int	min_args;
	int	max_args;	/* -1 = variadic */
	int	arg_types[3];
};

#define S9_PRIM    struct S9_primitive

/*
 * I/O
 */

#define s9_nl()		s9_prints("\n")

/*
 * Nested lists
 */

#define s9_car(x)          (S9_car[x])
#define s9_cdr(x)          (S9_cdr[x])
#define s9_caar(x)         (S9_car[S9_car[x]])
#define s9_cadr(x)         (S9_car[S9_cdr[x]])
#define s9_cdar(x)         (S9_cdr[S9_car[x]])
#define s9_cddr(x)         (S9_cdr[S9_cdr[x]])
#define s9_caaar(x)        (S9_car[S9_car[S9_car[x]]])
#define s9_caadr(x)        (S9_car[S9_car[S9_cdr[x]]])
#define s9_cadar(x)        (S9_car[S9_cdr[S9_car[x]]])
#define s9_caddr(x)        (S9_car[S9_cdr[S9_cdr[x]]])
#define s9_cdaar(x)        (S9_cdr[S9_car[S9_car[x]]])
#define s9_cdadr(x)        (S9_cdr[S9_car[S9_cdr[x]]])
#define s9_cddar(x)        (S9_cdr[S9_cdr[S9_car[x]]])
#define s9_cdddr(x)        (S9_cdr[S9_cdr[S9_cdr[x]]])
#define s9_caaaar(x)       (S9_car[S9_car[S9_car[S9_car[x]]]])
#define s9_caaadr(x)       (S9_car[S9_car[S9_car[S9_cdr[x]]]])
#define s9_caadar(x)       (S9_car[S9_car[S9_cdr[S9_car[x]]]])
#define s9_caaddr(x)       (S9_car[S9_car[S9_cdr[S9_cdr[x]]]])
#define s9_cadaar(x)       (S9_car[S9_cdr[S9_car[S9_car[x]]]])
#define s9_cadadr(x)       (S9_car[S9_cdr[S9_car[S9_cdr[x]]]])
#define s9_caddar(x)       (S9_car[S9_cdr[S9_cdr[S9_car[x]]]])
#define s9_cadddr(x)       (S9_car[S9_cdr[S9_cdr[S9_cdr[x]]]])
#define s9_cdaaar(x)       (S9_cdr[S9_car[S9_car[S9_car[x]]]])
#define s9_cdaadr(x)       (S9_cdr[S9_car[S9_car[S9_cdr[x]]]])
#define s9_cdadar(x)       (S9_cdr[S9_car[S9_cdr[S9_car[x]]]])
#define s9_cdaddr(x)       (S9_cdr[S9_car[S9_cdr[S9_cdr[x]]]])
#define s9_cddaar(x)       (S9_cdr[S9_cdr[S9_car[S9_car[x]]]])
#define s9_cddadr(x)       (S9_cdr[S9_cdr[S9_car[S9_cdr[x]]]])
#define s9_cdddar(x)       (S9_cdr[S9_cdr[S9_cdr[S9_car[x]]]])
#define s9_cddddr(x)       (S9_cdr[S9_cdr[S9_cdr[S9_cdr[x]]]])

/*
 * Access to fields of atoms
 */

#define s9_tag(n)		(S9_tag[n])

#define s9_string(n)		((char *) &Vectors[S9_cdr[n]])
#define s9_string_len(n)	(Vectors[S9_cdr[n] - 1])
#define s9_symbol_name(n)	(string(n))
#define s9_symbol_len(n)	(string_len(n))
#define s9_vector(n)		(&Vectors[S9_cdr[n]])
#define s9_vector_link(n)	(Vectors[S9_cdr[n] - 3])
#define s9_vector_index(n)	(Vectors[S9_cdr[n] - 2])
#define s9_vector_size(k)	(((k)+sizeof(s9_cell)-1) / \
                                 sizeof(s9_cell) + 3)
#define s9_vector_len(n)	(vector_size(string_len(n)) - 3)

#define s9_fixval(x)		cadr(x)
#define s9_small_int_value(x)	cadr(x)
#define s9_port_no(n)		(cadr(n))
#define s9_char_value(n)	(cadr(n))
#define s9_prim_slot(n)		(cadr(n))
#define s9_prim_info(n)		(&Primitives[prim_slot(n)])

/*
 * Type predicates
 */

#define s9_eof_p(n)		((n) == S9_END_OF_FILE)
#define s9_undefined_p(n)	((n) == S9_UNDEFINED)
#define s9_unspecific_p(n)	((n) == S9_UNSPECIFIC)

#define s9_boolean_p(n)	\
	((n) == S9_TRUE || (n) == S9_FALSE)

#define s9_integer_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && car(n) == S9_T_INTEGER)

#define s9_real_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && car(n) == S9_T_REAL)

#define s9_fix_p(n) \
        (!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && T_FIXNUM == car(n))

#define s9_number_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 (car(n) == S9_T_REAL || car(n) == S9_T_INTEGER))

#define s9_primitive_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 car(n) == S9_T_PRIMITIVE)

#define s9_function_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 car(n) == S9_T_FUNCTION)

#define s9_continuation_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 car(n) == S9_T_CONTINUATION)

#define s9_char_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && car(n) == S9_T_CHAR)

#define s9_syntax_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && car(n) == S9_T_SYNTAX)

#define s9_input_port_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 (tag(n) & S9_PORT_TAG) && car(n) == S9_T_INPUT_PORT)

#define s9_output_port_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_ATOM_TAG) && \
	 (tag(n) & S9_PORT_TAG) && car(n) == S9_T_OUTPUT_PORT)

#define s9_symbol_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_VECTOR_TAG) && car(n) == S9_T_SYMBOL)

#define s9_vector_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_VECTOR_TAG) && car(n) == S9_T_VECTOR)

#define s9_string_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_VECTOR_TAG) && car(n) == S9_T_STRING)

#define s9_constant_p(n) \
	(!s9_special_p(n) && (tag(n) & S9_CONST_TAG))

#define s9_atom_p(n) \
	(s9_special_p(n) || (tag(n) & S9_ATOM_TAG) || (tag(n) & S9_VECTOR_TAG))

#define s9_pair_p(x) (!s9_atom_p(x))

#define s9_small_int_p(n) (NIL == cddr(n))

#define s9_type_tag(n) \
	(S9_TRUE == (n)? S9_T_BOOLEAN: \
	 S9_FALSE == (n)? S9_T_BOOLEAN: \
	 (!s9_special_p(n) && (tag(n) & (S9_ATOM_TAG|S9_VECTOR_TAG))? car(n): \
	 S9_T_NONE))

/*
 * Allocators
 */

#define s9_cons(pa, pd)		s9_cons3((pa), (pd), 0)
#define s9_new_atom(pa, pd)	s9_cons3((pa), (pd), S9_ATOM_TAG)
#define s9_save(n)		(Stack = s9_cons((n), Stack))

/*
 * Bignum arithmetics
 */

#define s9_bignum_negative_p(a)	((cadr(a)) < 0)
#define s9_bignum_zero_p(a)	((cadr(a)) == 0)
#define s9_bignum_positive_p(a)	((cadr(a)) > 0)

/*
 * Real number structure
 */

#define S9_real_flags(x)	(cadr(x))
#define S9_real_exponent(x)	(caddr(x))
#define S9_real_mantissa(x)	(cdddr(x))

#define S9_REAL_NEGATIVE   0x01

#define S9_real_negative_flag(x)	(S9_real_flags(x) & S9_REAL_NEGATIVE)

/*
 * Real-number arithmetics
 */

#define S9_real_zero_p(x) \
	(car(S9_real_mantissa(x)) == 0 && cdr(S9_real_mantissa(x)) == S9_NIL)

#define S9_real_negative_p(x) \
	(S9_real_negative_flag(x) && !S9_real_zero_p(x))

#define S9_real_positive_p(x) \
	(!S9_real_negative_flag(x) && !S9_real_zero_p(x))

#define S9_real_negate(a) \
	S9_make_quick_real(S9_real_flags(a) & S9_REAL_NEGATIVE?	\
			S9_real_flags(a) & ~S9_REAL_NEGATIVE: \
			S9_real_flags(a) |  S9_REAL_NEGATIVE, \
			S9_real_exponent(a), S9_real_mantissa(a))

/*
 * Globals
 */

extern s9_cell	*S9_car,
		*S9_cdr;
extern char	*S9_tag;

extern s9_cell	*S9_vectors;

extern s9_cell	S9_stack;

extern s9_cell	*S9_gc_stack;
extern int	*S9_gc_stkptr;

extern S9_PRIM	*S9_primitives;

extern s9_cell	S9_zero,
		S9_one,
		S9_two,
		S9_ten;

extern s9_cell	S9_epsilon;

extern FILE	*S9_ports[];

extern int	S9_input_port,
		S9_output_port,
		S9_error_port;

/*
 * Prototypes
 */

#ifdef plan9
int	system(char *s);
#endif

void	s9_abort(void);
void	s9_add_image_vars(s9_cell **v);
s9_cell	s9_apply_prim(s9_cell f);
s9_cell	s9_argv_to_list(char **argv);
long	s9_asctol(char *s);
s9_cell	s9_bignum_abs(s9_cell a);
s9_cell	s9_bignum_add(s9_cell a, s9_cell b);
s9_cell	s9_bignum_divide(s9_cell a, s9_cell b);
int	s9_bignum_equal_p(s9_cell a, s9_cell b);
int	s9_bignum_even_p(s9_cell a);
int	s9_bignum_less_p(s9_cell a, s9_cell b);
s9_cell	s9_bignum_multiply(s9_cell a, s9_cell b);
s9_cell	s9_bignum_negate(s9_cell a);
s9_cell	s9_bignum_shift_left(s9_cell a, int fill);
s9_cell	s9_bignum_shift_right(s9_cell a);
s9_cell	s9_bignum_subtract(s9_cell a, s9_cell b);
s9_cell	s9_bignum_to_int(s9_cell x, int *of);
s9_cell	s9_bignum_to_real(s9_cell a);
s9_cell	s9_bignum_to_string(s9_cell x);
int	s9_blockread(char *s, int k);
void	s9_blockwrite(char *s, int k);
void	s9_close_port(int port);
void	s9_close_input_string(void);
s9_cell	s9_cons3(s9_cell pcar, s9_cell pcdr, int ptag);
int	s9_conses(s9_cell a);
void	s9_cons_stats(int x);
s9_cell	s9_copy_string(s9_cell x);
void	s9_count(s9_counter *c);
void	s9_countn(s9_counter *c, int n);
char	*s9_dump_image(char *path, char *magic);
int	s9_error_port(void);
void	s9_exponent_chars(char *s);
void	s9_fatal(char *msg);
s9_cell	s9_find_symbol(char *s);
s9_cell	s9_flat_copy(s9_cell n, s9_cell *lastp);
void	s9_flush(void);
int	s9_gc(void);
int	s9_gcv(void);
void	s9_gc_verbosity(int n);
void	s9_get_counters(s9_counter **nc, s9_counter **cc, s9_counter **vc,
			s9_counter **gc);
void	s9_mem_error_handler(void (*h)(int src));
void	s9_image_vars(s9_cell **v);
int	s9_input_port(void);
int	s9_inport_open_p(void);
int	s9_integer_string_p(char *s);
s9_cell	s9_intern_symbol(s9_cell y);
s9_cell	s9_int_to_bignum(int v);
int	s9_io_status(void);
void	s9_io_reset(void);
int	s9_length(s9_cell n);
char	*s9_load_image(char *path, char *magic);
int	s9_lock_port(int port);
s9_cell	s9_make_char(int c);
s9_cell	s9_make_integer(s9_cell i);
s9_cell	s9_make_norm_real(int flags, s9_cell exp, s9_cell mant);
s9_cell	s9_make_port(int portno, s9_cell type);
s9_cell	s9_make_primitive(S9_PRIM *p);
s9_cell	S9_make_real(int flags, s9_cell exp, s9_cell mant);
s9_cell	s9_make_real(int sign, s9_cell exp, s9_cell mant);
s9_cell	s9_make_string(char *s, int k);
s9_cell	s9_make_symbol(char *s, int k);
s9_cell	s9_make_vector(int k);
s9_cell	s9_mkfix(int i);
int	s9_new_port(void);
s9_cell	s9_new_vec(s9_cell type, int size);
int	s9_open_input_port(char *path);
char	*s9_open_input_string(char *s);
int	s9_open_output_port(char *path, int append);
int	s9_output_port(void);
int	s9_outport_open_p(void);
int	s9_port_eof(int p);
void	s9_prints(char *s);
int	s9_printer_limit(void);
void	s9_print_bignum(s9_cell n);
void	s9_print_expanded_real(s9_cell n);
void	s9_print_real(s9_cell n);
void	s9_print_sci_real(s9_cell n);
int	s9_readc(void);
s9_cell	s9_read_counter(s9_counter *c);
s9_cell	s9_real_abs(s9_cell a);
s9_cell	s9_real_add(s9_cell a, s9_cell b);
int	s9_real_approx_p(s9_cell a, s9_cell b);
s9_cell	s9_real_ceil(s9_cell x);
s9_cell	s9_real_divide(s9_cell a, s9_cell b);
int	s9_real_equal_p(s9_cell a, s9_cell b);
s9_cell	s9_real_exponent(s9_cell x);
s9_cell	s9_real_floor(s9_cell x);
s9_cell	s9_real_integer_p(s9_cell x);
int	s9_real_less_p(s9_cell a, s9_cell b);
s9_cell	s9_real_mantissa(s9_cell x);
s9_cell	s9_real_multiply(s9_cell a, s9_cell b);
s9_cell	s9_real_negate(s9_cell a);
s9_cell	s9_real_negative_p(s9_cell a);
s9_cell	s9_real_positive_p(s9_cell a);
s9_cell	s9_real_power(s9_cell x, s9_cell y);
s9_cell	s9_real_round(s9_cell x);
s9_cell	s9_real_sqrt(s9_cell x);
s9_cell	s9_real_subtract(s9_cell a, s9_cell b);
s9_cell	s9_real_to_bignum(s9_cell r);
s9_cell	s9_real_to_string(s9_cell r, int mode);
s9_cell	s9_real_trunc(s9_cell x);
s9_cell	s9_real_zero_p(s9_cell a);
void	s9_rejectc(int c);
void	s9_reset(void);
void	s9_reset_counter(s9_counter *c);
void	s9_reset_std_ports(void);
void	s9_run_stats(int x);
void	s9_fini(void);
void	s9_init(s9_cell **extroots, s9_cell *stack, int *stkptr);
s9_cell	s9_set_input_port(s9_cell port);
void	s9_set_node_limit(int k);
s9_cell	s9_set_output_port(s9_cell port);
void	s9_set_printer_limit(int k);
void	s9_set_vector_limit(int k);
int	s9_string_numeric_p(char *s);
s9_cell	s9_string_to_bignum(char *s);
s9_cell	s9_string_to_number(char *s);
s9_cell	s9_string_to_real(char *s);
s9_cell	s9_string_to_symbol(s9_cell x);
s9_cell	s9_symbol_ref(char *s);
s9_cell	s9_symbol_table(void);
s9_cell	s9_symbol_to_string(s9_cell x);
char	*s9_typecheck(s9_cell f);
int	s9_unlock_port(int port);
s9_cell	s9_unsave(int k);
void	s9_writec(int c);
