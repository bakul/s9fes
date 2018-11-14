/*
 * S9core Toolkit, Mk IVc
 * By Nils M Holm, 2007-2018
 * In the public domain
 *
 * Under jurisdictions without a public domain, the CC0 applies:
 * https://creativecommons.org/publicdomain/zero/1.0/
 */

#include "s9core.h"
#define S9_S9CORE
#include "s9import.h"
#include "s9ext.h"

/*
 * Global state
 */

static int	Cons_segment_size,
		Vec_segment_size;
static int	Cons_pool_size,
		Vec_pool_size;

static int	Verbose_GC = 0;

s9_cell		*Car,
		*Cdr;
char		*Tag;

s9_cell		*Vectors;
s9_cell		Nullvec;
s9_cell		Nullstr;
s9_cell		Blank;

cell		Stack;

static cell	Protect;
static int	Protp;

static cell	Free_list;
static cell	Free_vecs;

S9_PRIM		*Primitives;
static int	Last_prim,
		Max_prims;

static cell	Tmp_car,
		Tmp_cdr,
		Tmp;

static cell	Symbols;
static cell	Symhash;

static int	Printer_count,
		Printer_limit;

static int	IO_error;

FILE		*Ports[S9_MAX_PORTS];
static char	Port_flags[S9_MAX_PORTS];

int		Input_port,
		Output_port,
		Error_port;

static volatile int	Abort_flag;

static char	*Str_outport;
static int	Str_outport_len;
static char	*Str_inport;
static char	Rejected[2];

static long     Node_limit,
		Vector_limit;

static char	*Exponent_chars;
static cell	**Image_vars;

static void	(*Mem_error_handler)(int src);

/* Predefined bignum literals */
cell	Zero,
	One,
	Two,
	Ten;

/* Smallest value by which two real numbers can differ:
 * 10 ^ -(S9_MANTISSA_SIZE+1)
 */
cell	Epsilon;

/* Internal GC roots */
static cell	*GC_int_roots[] = {
			&Stack, &Symbols, &Symhash, &Tmp, &Tmp_car,
			&Tmp_cdr, &Zero, &One, &Two, &Ten, &Epsilon,
			&Nullvec, &Nullstr, &Blank, &Protect, NULL };

/* External GC roots */
static cell	**GC_ext_roots = NULL;

/* GC stack */
cell	*S9_gc_stack;
int	*S9_gc_stkptr;

/*
 * Internal vector representation
 */

#define RAW_VECTOR_LINK         0
#define RAW_VECTOR_INDEX        1
#define RAW_VECTOR_SIZE         2
#define RAW_VECTOR_DATA         3

/*
 * Internal node protection
 */

#ifdef S9_BITS_PER_WORD_64
 #define PROT_STACK_LEN	400
#else
 #define PROT_STACK_LEN	200
#endif

static void prot(cell x) {
	if (Protp >= PROT_STACK_LEN-1)
		s9_fatal("internal prot() stack overflow");
	vector(Protect)[++Protp] = x;
}

static cell unprot(int n) {
	cell	x;

	if (Protp - n < -1)
		s9_fatal("internal prot() stack underflow");
	x = vector(Protect)[Protp-n+1];
	while (n) {
		vector(Protect)[Protp--] = UNDEFINED;
		n--;
	}
	return x;
}

#define pref(n)	(vector(Protect)[Protp-(n)])

/*
 * Counting
 */

static int	Run_stats, Cons_stats;

static s9_counter	Conses,
			Nodes,
			Vecspace,
			Collections;

void s9_run_stats(int x) {
	Run_stats = x;
	if (Run_stats) {
		s9_reset_counter(&Nodes);
		s9_reset_counter(&Conses);
		s9_reset_counter(&Vecspace);
		s9_reset_counter(&Collections);
	}
}

void s9_cons_stats(int x) {
	Cons_stats = x;
}

void s9_reset_counter(s9_counter *c) {
	c->n = 0;
	c->n1k = 0;
	c->n1m = 0;
	c->n1g = 0;
	c->n1t = 0;
}

void s9_count(s9_counter *c) {
	c->n++;
	if (c->n >= 1000) {
		c->n -= 1000;
		c->n1k++;
		if (c->n1k >= 1000) {
			c->n1k -= 1000;
			c->n1m++;
			if (c->n1m >= 1000) {
				c->n1m -= 1000;
				c->n1g++;
				if (c->n1g >= 1000) {
					c->n1g -= 1000;
					c->n1t++;
				}
			}
		}
	}
}

void s9_countn(s9_counter *c, int n) {
	c->n += n;
	if (c->n >= 1000) {
		c->n1k += c->n / 1000;
		c->n = c->n % 1000;
		if (c->n1k >= 1000) {
			c->n1m += c->n1k / 1000;
			c->n1k = c->n1k % 1000;
			if (c->n1m >= 1000) {
				c->n1g += c->n1m / 1000;
				c->n1m = c->n1m % 1000;
				if (c->n1g >= 1000) {
					c->n1t += c->n1g / 1000;
					c->n1g = c->n1g % 1000;
				}
			}
		}
	}
}

cell s9_read_counter(s9_counter *c) {
	cell	n, m;

	n = s9_make_integer(c->n);
	n = cons(n, NIL);
	prot(n);
	m = s9_make_integer(c->n1k);
	n = cons(m, n);
	pref(0) = n;
	m = s9_make_integer(c->n1m);
	n = cons(m, n);
	pref(0) = n;
	m = s9_make_integer(c->n1g);
	n = cons(m, n);
	pref(0) = n;
	m = s9_make_integer(c->n1t);
	n = cons(m, n);
	unprot(1);
	return n;
}

void s9_get_counters(s9_counter **nc, s9_counter **cc, s9_counter **vc,
			s9_counter **gc) {
	*nc = &Nodes;
	*cc = &Conses;
	*vc = &Vecspace;
	*gc = &Collections;
}

/*
 * Raw I/O
 */

int s9_inport_open_p(void) {
	return Ports[Input_port] != NULL;
}

int s9_outport_open_p(void) {
	return Ports[Output_port] != NULL;
}

int s9_readc(void) {
	int	c, i;

	if (Str_inport != NULL) {
		for (i=1; i>=0; i--) {
			if (Rejected[i] > -1) {
				c = Rejected[i];
				Rejected[i] = -1;
				return c;
			}
		}
		if (0 == *Str_inport) {
			return EOF;
		}
		else {
			return *Str_inport++;
		}
	}
	else {
		if (!s9_inport_open_p())
			s9_fatal("s9_readc(): input port is not open");
		return getc(Ports[Input_port]);
	}
}

void s9_rejectc(int c) {
	if (Str_inport != NULL) {
		if (Rejected[0] == -1)
			Rejected[0] = c;
		else
			Rejected[1] = c;
	}
	else {
		ungetc(c, Ports[Input_port]);
	}
}

void s9_writec(int c) {
	if (!s9_outport_open_p())
		s9_fatal("s9_writec(): output port is not open");
	(void) putc(c, Ports[Output_port]);
}

char *s9_open_input_string(char *s) {
	char	*os;

	os = Str_inport;
	Str_inport = s;
	Rejected[0] = Rejected[1] = -1;
	return os;
}

void s9_close_input_string(void) {
	Str_inport = NULL;
}

void s9_flush(void) {
	if (fflush(Ports[Output_port]))
		IO_error = 1;
}

void s9_set_printer_limit(int k) {
	Printer_limit = k;
	Printer_count = 0;
}

int s9_printer_limit(void) {
	return Printer_limit && Printer_count >= Printer_limit;
}

void s9_blockwrite(char *s, int k) {
	if (Str_outport) {
		if (k >= Str_outport_len) {
			k = Str_outport_len;
			IO_error = 1;
		}
		memcpy(Str_outport, s, k);
		Str_outport += k;
		Str_outport_len -= k;
		*Str_outport = 0;
		return;
	}
	if (!s9_outport_open_p())
		s9_fatal("s9_blockwrite(): output port is not open");
	if (Printer_limit && Printer_count > Printer_limit) {
		if (Printer_limit > 0)
			fwrite("...", 1, 3, Ports[Output_port]);
		Printer_limit = -1;
		return;
	}
	if (fwrite(s, 1, k, Ports[Output_port]) != k)
		IO_error = 1;
	if (Output_port == 1 && s[k-1] == '\n')
		s9_flush();
	Printer_count += k;
}

int s9_blockread(char *s, int k) {
	int	n;

	if (!s9_inport_open_p())
		s9_fatal("s9_blockread(): input port is not open");
	n = fread(s, 1, k, Ports[Input_port]);
	if (n < 0) IO_error = 1;
	return n;
}

void s9_prints(char *s) {
	s9_blockwrite(s, strlen(s));
}

int s9_io_status(void) {
	return IO_error? -1: 0;
}

void s9_io_reset(void) {
	IO_error = 0;
}

/*
 * Error Handling
 */

void s9_fatal(char *msg) {
	fprintf(stderr, "S9core: fatal error: ");
	fprintf(stderr, "%s\n", msg);
	bye(1);
}

void s9_abort(void) {
	Abort_flag = 1;
}

void s9_reset(void) {
	Abort_flag = 0;
}

/*
 * Memory Management
 */

void s9_set_node_limit(int n) {
	Node_limit = n * 1024L;
}

void s9_set_vector_limit(int n) {
	Vector_limit = n * 1024L;
}

void s9_gc_verbosity(int n) {
	Verbose_GC = n;
}

void s9_mem_error_handler(void (*h)(int src)) {
	Mem_error_handler = h;
}

static void new_cons_segment(void) {
	Car = realloc(Car, sizeof(cell)*(Cons_pool_size+Cons_segment_size));
	Cdr = realloc(Cdr, sizeof(cell)*(Cons_pool_size+Cons_segment_size));
	Tag = realloc(Tag, Cons_pool_size + Cons_segment_size);
	if (Car == NULL || Cdr == NULL || Tag == NULL)
		s9_fatal("new_cons_segment: out of physical memory");
	memset(&car(Cons_pool_size), 0, Cons_segment_size * sizeof(cell));
	memset(&cdr(Cons_pool_size), 0, Cons_segment_size * sizeof(cell));
	memset(&Tag[Cons_pool_size], 0, Cons_segment_size);
	Cons_pool_size += Cons_segment_size;
	Cons_segment_size = Cons_pool_size / 2;
}

static void new_vec_segment(void) {
	Vectors = realloc(Vectors, sizeof(cell) *
			(Vec_pool_size + Vec_segment_size));
	if (Vectors == NULL)
		s9_fatal("new_vec_segment: out of physical memory");
	memset(&Vectors[Vec_pool_size], 0, Vec_segment_size * sizeof(cell));
	Vec_pool_size += Vec_segment_size;
	Vec_segment_size = Vec_pool_size / 2;
}

/*
 * Mark nodes which can be accessed through N.
 * Using the Deutsch/Schorr/Waite pointer reversal algorithm.
 * S0: M==0, S==0, unvisited, process CAR (vectors: process 1st slot);
 * S1: M==1, S==1, CAR visited, process CDR (vectors: process next slot);
 * S2: M==1, S==0, completely visited, return to parent.
 */

static void mark(cell n) {
	cell	p, parent, *v;
	int	i;

	parent = NIL;
	while (1) {
		if (s9_special_p(n) || (Tag[n] & S9_MARK_TAG)) {
			if (parent == NIL)
				break;
			if (Tag[parent] & S9_VECTOR_TAG) { /* S1 --> S1|done */
				i = vector_index(parent);
				v = vector(parent);
				if (Tag[parent] & S9_STATE_TAG &&
				    i+1 < vector_len(parent)
				) {			/* S1 --> S1 */
					p = v[i+1];
					v[i+1] = v[i];
					v[i] = n;
					n = p;
					vector_index(parent) = i+1;
				}
				else {			/* S1 --> done */
					Tag[parent] &= ~S9_STATE_TAG;
					p = parent;
					parent = v[i];
					v[i] = n;
					n = p;
				}
			}
			else if (Tag[parent] & S9_STATE_TAG) {	/* S1 --> S2 */
				p = cdr(parent);
				cdr(parent) = car(parent);
				car(parent) = n;
				Tag[parent] &= ~S9_STATE_TAG;
				/* Tag[parent] |=  S9_MARK_TAG; */
				n = p;
			}
			else {				/* S2 --> done */
				p = parent;
				parent = cdr(p);
				cdr(p) = n;
				n = p;
			}
		}
		else if (Tag[n] & S9_VECTOR_TAG) {	/* S0 --> S1|S2 */
			Tag[n] |= S9_MARK_TAG;
			/* Tag[n] &= ~S9_STATE_TAG; */
			vector_link(n) = n;
			if (car(n) == T_VECTOR && vector_len(n) != 0) {
				Tag[n] |= S9_STATE_TAG;
				vector_index(n) = 0;
				v = vector(n);
				p = v[0];
				v[0] = parent;
				parent = n;
				n = p;
			}
		}
		else if (Tag[n] & S9_ATOM_TAG) {	/* S0 --> S2 */
			if (input_port_p(n) || output_port_p(n))
				Port_flags[port_no(n)] |= S9_USED_TAG;
			p = cdr(n);
			cdr(n) = parent;
			/*Tag[n] &= ~S9_STATE_TAG;*/
			parent = n;
			n = p;
			Tag[parent] |= S9_MARK_TAG;
		}
		else {					/* S0 --> S1 */
			p = car(n);
			car(n) = parent;
			Tag[n] |= S9_MARK_TAG;
			parent = n;
			n = p;
			Tag[parent] |= S9_STATE_TAG;
		}
	}
}

/* Mark and sweep GC. */
int s9_gc(void) {
	int	i, k, sk = 0;
	char	buf[100];

	if (Run_stats)
		s9_count(&Collections);
	for (i=0; i<S9_MAX_PORTS; i++) {
		if (Port_flags[i] & S9_LOCK_TAG)
			Port_flags[i] |= S9_USED_TAG;
		else
			Port_flags[i] &= ~S9_USED_TAG;
	}
	if (GC_stack && *GC_stack != NIL) {
		sk = string_len(*GC_stack);
		string_len(*GC_stack) = (1 + *GC_stkptr) * sizeof(cell);
	}
	for (i=0; GC_int_roots[i] != NULL; i++) {
		mark(*GC_int_roots[i]);
	}
	if (GC_ext_roots) {
		for (i=0; GC_ext_roots[i] != NULL; i++)
			mark(*GC_ext_roots[i]);
	}
	if (GC_stack && *GC_stack != NIL) {
		string_len(*GC_stack) = sk;
	}
	k = 0;
	Free_list = NIL;
	for (i=0; i<Cons_pool_size; i++) {
		if (!(Tag[i] & S9_MARK_TAG)) {
			cdr(i) = Free_list;
			Free_list = i;
			k++;
		}
		else {
			Tag[i] &= ~S9_MARK_TAG;
		}
	}
	for (i=0; i<S9_MAX_PORTS; i++) {
		if (!(Port_flags[i] & S9_USED_TAG) && Ports[i] != NULL) {
			fclose(Ports[i]);
			Ports[i] = NULL;
		}
	}
	if (Verbose_GC > 1) {
		sprintf(buf, "GC: %d nodes reclaimed", k);
		s9_prints(buf); nl();
		s9_flush();
	}
	return k;
}

/* Allocate a fresh node and initialize with PCAR,PCDR,PTAG. */
cell s9_cons3(cell pcar, cell pcdr, int ptag) {
	cell	n;
	int	k;
	char	buf[100];

	if (Run_stats) {
		s9_count(&Nodes);
		if (	Cons_stats &&
			0 == (ptag & (S9_ATOM_TAG|S9_VECTOR_TAG|S9_PORT_TAG))
		)
			s9_count(&Conses);
	}
	if (Free_list == NIL) {
		if (ptag == 0)
			Tmp_car = pcar;
		if (!(ptag & S9_VECTOR_TAG))
			Tmp_cdr = pcdr;
		k = s9_gc();
		/*
		 * Performance increases dramatically if we
		 * do not wait for the pool to run dry.
		 * In fact, don't even let it come close to that.
		 */
		if (k < Cons_pool_size / 2) {
			if (	Node_limit &&
				Cons_pool_size + Cons_segment_size
					> Node_limit
			) {
				if (Mem_error_handler)
					(*Mem_error_handler)(1);
				else
					s9_fatal("s9_cons3: hit memory limit");
			}
			else {
				new_cons_segment();
				if (Verbose_GC) {
					sprintf(buf,
						"GC: new segment,"
						 " nodes = %d,"
						 " next segment = %d",
						Cons_pool_size,
						Cons_segment_size);
					s9_prints(buf); nl();
					s9_flush();
				}
				s9_gc();
			}
		}
		Tmp_car = Tmp_cdr = NIL;
	}
	if (Free_list == NIL)
		s9_fatal(
		  "s9_cons3: failed to recover from low memory condition");
	n = Free_list;
	Free_list = cdr(Free_list);
	car(n) = pcar;
	cdr(n) = pcdr;
	Tag[n] = ptag;
	return n;
}

/* Mark all vectors unused */
static void unmark_vectors(void) {
	int	p, k, link;

	p = 0;
	while (p < Free_vecs) {
		link = p;
		k = Vectors[p + RAW_VECTOR_SIZE];
		p += vector_size(k);
		Vectors[link] = NIL;
	}
}

/* In situ vector pool garbage collection and compaction */
int s9_gcv(void) {
	int	v, k, to, from;
	char	buf[100];

	unmark_vectors();
	s9_gc();		/* re-mark live vectors */
	to = from = 0;
	while (from < Free_vecs) {
		v = Vectors[from + RAW_VECTOR_SIZE];
		k = vector_size(v);
		if (Vectors[from + RAW_VECTOR_LINK] != NIL) {
			if (to != from) {
				memmove(&Vectors[to], &Vectors[from],
					k * sizeof(cell));
				cdr(Vectors[to + RAW_VECTOR_LINK]) =
					to + RAW_VECTOR_DATA;
			}
			to += k;
		}
		from += k;
	}
	k = Free_vecs - to;
	if (Verbose_GC > 1) {
		sprintf(buf, "GC: gcv: %d cells reclaimed", k);
		s9_prints(buf); nl();
		s9_flush();
	}
	Free_vecs = to;
	return k;
}

/* Allocate vector from pool */
cell s9_new_vec(cell type, int size) {
	cell	n;
	int	i, v, wsize;
	char	buf[100];

	wsize = vector_size(size);
	if (Run_stats) {
		s9_countn(&Vecspace, wsize);
	}
	if (Free_vecs + wsize >= Vec_pool_size) {
		s9_gcv();
		while (	Free_vecs + wsize >=
			Vec_pool_size - Vec_pool_size / 2
		) {
			if (	Vector_limit &&
				Vec_pool_size + Vec_segment_size
					> Vector_limit
			) {
				if (Mem_error_handler)
					(*Mem_error_handler)(2);
				else
					s9_fatal("new_vec: hit memory limit");
				break;
			}
			else {
				new_vec_segment();
				s9_gcv();
				if (Verbose_GC) {
					sprintf(buf,
						"GC: new_vec: new segment,"
						 " cells = %d",
						Vec_pool_size);
					s9_prints(buf); nl();
					s9_flush();
				}
			}
		}
	}
	if (Free_vecs + wsize >= Vec_pool_size)
		s9_fatal(
		  "new_vec: failed to recover from low memory condition");
	v = Free_vecs;
	Free_vecs += wsize;
	n = s9_cons3(type, v + RAW_VECTOR_DATA, S9_VECTOR_TAG);
	Vectors[v + RAW_VECTOR_LINK] = n;
	Vectors[v + RAW_VECTOR_INDEX] = 0;
	Vectors[v + RAW_VECTOR_SIZE] = size;
	if (type == T_VECTOR) {
		for (i = RAW_VECTOR_DATA; i<wsize; i++)
			Vectors[v+i] = UNDEFINED;
	}
	return n;
}

/* Pop K nodes off the Stack, return last one. */
cell s9_unsave(int k) {
	cell	n = NIL; /*LINT*/

	while (k) {
		if (Stack == NIL)
			s9_fatal("s9_unsave: stack underflow");
		n = car(Stack);
		Stack = cdr(Stack);
		k--;
	}
	return n;
}

static unsigned hash(char *s) {
	unsigned int	h = 0;

	while (*s) h = ((h<<5)+h) ^ *s++;
	return h;
}

static int hash_size(int n) {
	if (n < 47) return 47;
	if (n < 97) return 97;
	if (n < 199) return 199;
	if (n < 499) return 499;
	if (n < 997) return 997;
	if (n < 9973) return 9973;
	if (n < 19997) return 19997;
	return 39989;
}

#define intval(x) cadr(x)

static void rehash_symbols(void) {
	unsigned int	i;
	cell		*v, n, p, new;
	unsigned int	h, k;

	if (NIL == Symhash)
		k = hash_size(s9_length(Symbols));
	else
		k = hash_size(intval(vector(Symhash)[0]));
	Symhash = s9_new_vec(T_VECTOR, (k+1) * sizeof(cell));
	v = vector(Symhash);
	for (i=1; i<=k; i++) v[i] = NIL;
	i = 0;
	for (p = Symbols; p != NIL; p = cdr(p)) {
		h = hash(symbol_name(car(p)));
		n = cons(car(p), NIL);
		n = cons(n, vector(Symhash)[h%k+1]);
		vector(Symhash)[h%k+1] = n;
		i++;
	}
	new = s9_make_integer(i);
	vector(Symhash)[0] = new;
}

void add_symhash(cell x) {
	cell		n, new;
	unsigned int	h, i, k;

	if (NIL == Symhash) {
		rehash_symbols();
		return;
	}
	i = intval(vector(Symhash)[0]);
	k = vector_len(Symhash)-1;
	if (i > k) {
		rehash_symbols();
		return;
	}
	h = hash(symbol_name(x));
	n = cons(x, NIL);
	n = cons(n, vector(Symhash)[h%k+1]);
	vector(Symhash)[h%k+1] = n;
	new = s9_make_integer(i+1);
	vector(Symhash)[0] = new;
}

cell s9_find_symbol(char *s) {
	unsigned int	h, k;
	cell		n;

	if (NIL == Symhash) return NIL;
	k = vector_len(Symhash)-1;
	h = hash(s);
	for (n = vector(Symhash)[h%k+1]; n != NIL; n = cdr(n))
		if (!strcmp(s, symbol_name(caar(n))))
			return caar(n);
	return NIL;
}

/*
cell s9_find_symbol(char *s) {
	cell	y;

	y = Symbols;
	while (y != NIL) {
		if (!strcmp(symbol_name(car(y)), s))
			return car(y);
		y = cdr(y);
	}
	return NIL;
}
*/

cell s9_make_symbol(char *s, int k) {
	cell	n;

	n = s9_new_vec(T_SYMBOL, k+1);
	strcpy(symbol_name(n), s);
	return n;
}

cell s9_intern_symbol(cell y) {
	Symbols = cons(y, Symbols);
	add_symhash(y);
	return y;
}

cell s9_symbol_table(void) {
	return Symbols;
}

cell s9_symbol_ref(char *s) {
	cell	y, new;

	y = s9_find_symbol(s);
	if (y != NIL)
		return y;
	new = s9_make_symbol(s, strlen(s));
	return s9_intern_symbol(new);
}

cell s9_make_string(char *s, int k) {
	cell	n;

	if (0 == k) return Nullstr;
	n = s9_new_vec(T_STRING, k+1);
	strncpy(string(n), s, k+1);
	return n;
}

cell s9_make_vector(int k) {
	if (0 == k) return Nullvec;
	return s9_new_vec(T_VECTOR, k * sizeof(cell));
}

cell s9_mkfix(int v) {
	cell	n;

	n = new_atom(v, NIL);
	return new_atom(T_FIXNUM, n);
}

cell s9_make_integer(cell i) {
	cell	n;

	switch (i) {
	case 0:		return Zero;
	case 1:		return One;
	case 2:		return Two;
	case 10:	return Ten;
	default:
		n = new_atom(i, NIL);
		return new_atom(T_INTEGER, n);
	}
}

static cell make_init_integer(cell i) {
	cell	n;

	n = new_atom(i, NIL);
	return new_atom(T_INTEGER, n);
}

cell s9_make_char(int x) {
	cell n;

	if (' ' == x) return Blank;
	n = new_atom(x & 0xff, NIL);
	return new_atom(T_CHAR, n);
}

static cell real_normalize(cell x);

static cell S9_make_quick_real(int flags, cell exp, cell mant) {
	cell	n;

	n = new_atom(exp, mant);
	n = new_atom(flags, n);
	n = new_atom(T_REAL, n);
	return n;
}

cell S9_make_real(int flags, cell exp, cell mant) {
	cell	r;

	prot(mant);
	r = S9_make_quick_real(flags, exp, mant);
	r = real_normalize(r);
	unprot(1);
	return r;
}

cell s9_make_real(int sign, cell exp, cell mant) {
	cell	m;
	int	i;

	i = 0;
	for (m = cdr(mant); m != NIL; m = cdr(m))
		i++;
	if (i > S9_MANTISSA_SIZE)
		return UNDEFINED;
	return S9_make_real(sign < 0? REAL_NEGATIVE: 0, exp, cdr(mant));
}

static void grow_primitives(void) {
	Max_prims += S9_PRIM_SEG_SIZE;
	Primitives = (S9_PRIM *) realloc(Primitives,
					sizeof(S9_PRIM) * Max_prims);
	if (Primitives == NULL)
		s9_fatal("grow_primitives: out of physical memory");
}

cell s9_make_primitive(S9_PRIM *p) {
	cell	n;

	n = new_atom(Last_prim, NIL);
	n = new_atom(T_PRIMITIVE, n);
	if (Last_prim >= Max_prims)
		grow_primitives();
	memcpy(&Primitives[Last_prim], p, sizeof(S9_PRIM));
	Last_prim++;
	return n;
}

cell s9_make_port(int portno, cell type) {
	cell	n;
	int	pf;

	pf = Port_flags[portno];
	Port_flags[portno] |= S9_LOCK_TAG;
	n = new_atom(portno, NIL);
	n = s9_cons3(type, n, S9_ATOM_TAG|S9_PORT_TAG);
	Port_flags[portno] = pf;
	return n;
}

cell s9_string_to_symbol(cell x) {
	cell	y, n, k;

	y = s9_find_symbol(string(x));
	if (y != NIL)
		return y;
	/*
	 * Cannot pass content to s9_make_symbol(), because
	 * string(x) may move during GC.
	 */
	k = string_len(x);
	n = s9_make_symbol("", k-1);
	memcpy(symbol_name(n), string(x), k);
	return s9_intern_symbol(n);
}

cell s9_symbol_to_string(cell x) {
	cell	n, k;

	/*
	 * Cannot pass name to s9_make_string(), because
	 * symbol_name(x) may move during GC.
	 */
 	k = symbol_len(x);
	n = s9_make_string("", k-1);
	memcpy(string(n), symbol_name(x), k);
	return n;
}

cell s9_copy_string(cell x) {
	cell	n, k;

	/*
	 * See s9_string_to_symbol(), above.
	 */
 	k = string_len(x);
	n = s9_make_string("", k-1);
	memcpy(string(n), string(x), k);
	return n;
}

/*
 * Miscellanea
 */

int s9_length(cell n) {
	int	k;

	for (k = 0; n != NIL; n = cdr(n))
		k++;
	return k;
}

int s9_conses(cell n) {
	int	k;

	for (k = 0; pair_p(n); n = cdr(n))
		k++;
	return k;
}

cell s9_flat_copy(cell n, cell *lastp) {
	cell	a, m, last, new;

	if (n == NIL) {
		if (lastp != NULL)
			lastp[0] = NIL;
		return NIL;
	}
	m = s9_cons3(NIL, NIL, Tag[n]);
	prot(m);
	a = m;
	last = m;
	while (n != NIL) {
		car(a) = car(n);
		last = a;
		n = cdr(n);
		if (n != NIL) {
			new = s9_cons3(NIL, NIL, Tag[n]);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	unprot(1);
	if (lastp != NULL)
		lastp[0] = last;
	return m;
}

long s9_asctol(char *s) {
	while (*s == '0' && s[1])
		s++;
	return atol(s);
}

static char *ntoa(char *b, cell x, int w) {
	char	buf[40];
	int	i = 0, neg = 0;
	char	*p = &buf[sizeof(buf)-1];

	if (x < 0) {
		x = -x;
		neg = 1;
	}
	*p = 0;
	while (x || i == 0) {
		i++;
		if (i >= sizeof(buf)-1)
			s9_fatal("ntoa: number too big");
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
			s9_fatal("ntoa: number too big");
		p--;
		*p = '-';
	}
	strcpy(b, p);
	return b;
}

cell s9_argv_to_list(char **argv) {
	int	i;
	cell	a, n;

	if (argv[0] == NULL) return NIL;
	a = cons(NIL, NIL);
	prot(a);
	for (i = 0; argv[i] != NULL; i++) {
		n = s9_make_string(argv[i], strlen(argv[i]));
		car(a) = n;
		if (argv[i+1] != NULL) {
			n = cons(NIL, NIL);
			cdr(a) = n;
			a = cdr(a);
		}
	}
	return unprot(1);
}

#ifdef plan9

int system(char *cmd) {
	Waitmsg	*w;
	int	pid;
	char	*argv[] = { "/bin/rc", "-c", cmd, NULL };
	
	switch (pid = fork()) {
	case -1:
		return -1;
	case 0:
		exec(argv[0], argv);
		bye(1);
	default:
		while ((w = wait()) != NULL) {
			if (w->pid == pid) {
				if (w->msg[0] == 0) {
					free(w);
					return 0;
				}
				free(w);
				return 1;
			}
			free(w);
		}
		return 0;
	}
}

#endif /* plan9 */

/*
 * Bignums
 */

cell s9_bignum_abs(cell a) {
	cell	n;

	prot(a);
	n = new_atom(labs(cadr(a)), cddr(a));
	n = new_atom(T_INTEGER, n);
	unprot(1);
	return n;
}

cell s9_bignum_negate(cell a) {
	cell	n;

	prot(a);
	n = new_atom(-cadr(a), cddr(a));
	n = new_atom(T_INTEGER, n);
	unprot(1);
	return n;
}

static cell reverse_segments(cell n) {
	cell	m;

	m = NIL;
	while (n != NIL) {
		m = new_atom(car(n), m);
		n = cdr(n);
	}
	return m;
}

int s9_bignum_even_p(cell a) {
	while (cdr(a) != NIL)
		a = cdr(a);
	return car(a) % 2 == 0;
}

cell s9_bignum_add(cell a, cell b);
cell s9_bignum_subtract(cell a, cell b);

static cell Bignum_add(cell a, cell b) {
	cell	fa, fb, result, r;
	int	carry;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A+-B --> -(|A|+|B|) */
			a = s9_bignum_abs(a);
			prot(a);
			a = s9_bignum_add(a, s9_bignum_abs(b));
			unprot(1);
			return s9_bignum_negate(a);
		}
		else {
			/* -A+B --> B-|A| */
			return s9_bignum_subtract(b, s9_bignum_abs(a));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A+-B --> A-|B| */
		return s9_bignum_subtract(a, s9_bignum_abs(b));
	}
	/* A+B */
	a = reverse_segments(cdr(a));
	prot(a);
	b = reverse_segments(cdr(b));
	prot(b);
	carry = 0;
	result = NIL;
	prot(result);
	while (a != NIL || b != NIL || carry) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa + fb + carry;
		carry = 0;
		if (r >= S9_INT_SEG_LIMIT) {
			r -= S9_INT_SEG_LIMIT;
			carry = 1;
		}
		result = new_atom(r, result);
		pref(0) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unprot(3);
	return new_atom(T_INTEGER, result);
}

cell s9_bignum_add(cell a, cell b) {
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	a = Bignum_add(a, b);
	unprot(2);
	return a;
}

int s9_bignum_less_p(cell a, cell b) {
	int	ka, kb, neg_a, neg_b;

	neg_a = bignum_negative_p(a);
	neg_b = bignum_negative_p(b);
	if (neg_a && !neg_b) return 1;
	if (!neg_a && neg_b) return 0;
	ka = s9_length(a);
	kb = s9_length(b);
	if (ka < kb) return neg_a? 0: 1;
	if (ka > kb) return neg_a? 1: 0;
	Tmp = b;
	a = s9_bignum_abs(a);
	prot(a);
	b = s9_bignum_abs(b);
	unprot(1);
	Tmp = NIL;
	a = cdr(a);
	b = cdr(b);
	while (a != NIL) {
		if (car(a) < car(b)) return neg_a? 0: 1;
		if (car(a) > car(b)) return neg_a? 1: 0;
		a = cdr(a);
		b = cdr(b);
	}
	return 0;
}

int s9_bignum_equal_p(cell a, cell b) {
	a = cdr(a);
	b = cdr(b);
	while (a != NIL && b != NIL) {
		if (car(a) != car(b))
			return 0;
		a = cdr(a);
		b = cdr(b);
	}
	return a == NIL && b == NIL;
}

static cell Bignum_subtract(cell a, cell b) {
	cell	fa, fb, result, r;
	int	borrow;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A--B --> -A+|B| --> |B|-|A| */
			a = s9_bignum_abs(a);
			prot(a);
			a = s9_bignum_subtract(s9_bignum_abs(b), a);
			unprot(1);
			return a;
		}
		else {
			/* -A-B --> -(|A|+B) */
			return s9_bignum_negate(
				s9_bignum_add(s9_bignum_abs(a), b));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A--B --> A+|B| */
		return s9_bignum_add(a, s9_bignum_abs(b));
	}
	/* A-B, A<B --> -(B-A) */
	if (s9_bignum_less_p(a, b))
		return s9_bignum_negate(s9_bignum_subtract(b, a));
	/* A-B, A>=B */
	a = reverse_segments(cdr(a));
	prot(a);
	b = reverse_segments(cdr(b));
	prot(b);
	borrow = 0;
	result = NIL;
	prot(result);
	while (a != NIL || b != NIL || borrow) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa - fb - borrow;
		borrow = 0;
		if (r < 0) {
			r += S9_INT_SEG_LIMIT;
			borrow = 1;
		}
		result = new_atom(r, result);
		pref(0) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unprot(3);
	while (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	return new_atom(T_INTEGER, result);
}

cell s9_bignum_subtract(cell a, cell b) {
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	a = Bignum_subtract(a, b);
	unprot(2);
	return a;
}

cell s9_bignum_shift_left(cell a, int fill) {
	cell	r, c, result;
	int	carry;

	prot(a);
	a = reverse_segments(cdr(a));
	prot(a);
	carry = fill;
	result = NIL;
	prot(result);
	while (a != NIL) {
		if (car(a) >= S9_INT_SEG_LIMIT/10) {
			c = car(a) / (S9_INT_SEG_LIMIT/10);
			r = car(a) % (S9_INT_SEG_LIMIT/10) * 10;
			r += carry;
			carry = c;
		}
		else {
			r = car(a) * 10 + carry;
			carry = 0;
		}
		result = new_atom(r, result);
		pref(0) = result;
		a = cdr(a);
	}
	if (carry)
		result = new_atom(carry, result);
	result = new_atom(T_INTEGER, result);
	unprot(3);
	return result;
}

/* Result: (a/10 . a%10) */
cell s9_bignum_shift_right(cell a) {
	cell	r, c, result;
	int	carry;

	prot(a);
	a = cdr(a);
	prot(a);
	carry = 0;
	result = NIL;
	prot(result);
	while (a != NIL) {
		c = car(a) % 10;
		r = car(a) / 10;
		r += carry * (S9_INT_SEG_LIMIT/10);
		carry = c;
		result = new_atom(r, result);
		pref(0) = result;
		a = cdr(a);
	}
	result = reverse_segments(result);
	if (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	result = new_atom(T_INTEGER, result);
	pref(0) = result;
	carry = s9_make_integer(carry);
	result = cons(result, carry);
	unprot(3);
	return result;
}

cell s9_bignum_multiply(cell a, cell b) {
	int	neg;
	cell	r, i, result;

	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	neg = bignum_negative_p(a) != bignum_negative_p(b);
	a = s9_bignum_abs(a);
	prot(a);
	b = s9_bignum_abs(b);
	prot(b);
	result = Zero;
	prot(result);
	while (!bignum_zero_p(a)) {
		r = s9_bignum_shift_right(a);
		i = caddr(r);
		a = car(r);
		pref(2) = a;
		while (i) {
			if (Abort_flag) {
				unprot(5);
				return Zero;
			}
			result = s9_bignum_add(result, b);
			pref(0) = result;
			i--;
		}
		b = s9_bignum_shift_left(b, 0);
		pref(1) = b;
	}
	if (neg)
		result = s9_bignum_negate(result);
	unprot(5);
	return result;
}

/*
 * Equalize A and B, e.g.:
 * A=123, B=12345 --> 12300, 100
 * Return (scaled-a . scaling-factor)
 */
static cell bignum_equalize(cell a, cell b) {
	cell	r, f, r0, f0;

	r0 = a;
	prot(r0);
	f0 = One;
	prot(f0);
	r = r0;
	prot(r);
	f = f0;
	prot(f);
	while (s9_bignum_less_p(r, b)) {
		pref(3) = r0 = r;
		pref(2) = f0 = f;
		r = s9_bignum_shift_left(r, 0);
		pref(1) = r;
		f = s9_bignum_shift_left(f, 0);
		pref(0) = f;
	}
	unprot(4);
	return cons(r0, f0);
}

/* Result: (a/b . a%b) */
static cell Bignum_divide(cell a, cell b) {
	int	neg, neg_a;
	cell	result, f;
	int	i;
	cell	c, c0;

	neg_a = bignum_negative_p(a);
	neg = neg_a != bignum_negative_p(b);
	a = s9_bignum_abs(a);
	prot(a);
	b = s9_bignum_abs(b);
	prot(b);
	if (s9_bignum_less_p(a, b)) {
		if (neg_a)
			a = s9_bignum_negate(a);
		unprot(2);
		return cons(Zero, a);
	}
	b = bignum_equalize(b, a);
	pref(1) = b;
	pref(0) = a;
	c = NIL;
	prot(c);
	c0 = NIL;
	prot(c0);
	f = cdr(b);
	b = car(b);
	pref(3) = b;
	prot(f);
	result = Zero;
	prot(result);
	while (!bignum_zero_p(f)) {
		c = Zero;
		pref(3) = c;
		pref(2) = c0 = c;
		i = 0;
		while (!s9_bignum_less_p(a, c)) {
			pref(2) = c0 = c;
			c = s9_bignum_add(c, b);
			pref(3) = c;
			i++;
		}
		result = s9_bignum_shift_left(result, i-1);
		pref(0) = result;
		a = s9_bignum_subtract(a, c0);
		pref(4) = a;
		f = s9_bignum_shift_right(f);
		f = car(f);
		pref(1) = f;
		b = s9_bignum_shift_right(b);
		b = car(b);
		pref(5) = b;
	}
	if (neg)
		result = s9_bignum_negate(result);
	pref(0) = result;
	if (neg_a)
		a = s9_bignum_negate(a);
	unprot(6);
	return cons(result, a);
}

cell s9_bignum_divide(cell a, cell b) {
	if (bignum_zero_p(b))
		return UNDEFINED;
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	a = Bignum_divide(a, b);
	unprot(2);
	return a;
}

/*
 * Real Number Arithmetics
 */

static cell count_digits(cell m) {
	int	k;
	cell	x;

	x = car(m);
	k = 0;
	while (x != 0) {
		x /= 10;
		k++;
	}
	k = k==0? 1: k;
	m = cdr(m);
	while (m != NIL) {
		k += S9_DIGITS_PER_CELL;
		m = cdr(m);
	}
	return k;
}

cell s9_real_exponent(cell x) {
	if (integer_p(x)) return 0;
	return Real_exponent(x);
}

cell s9_real_mantissa(cell x) {
	cell	m;

	if (integer_p(x))
		return x;
	m = new_atom(T_INTEGER, Real_mantissa(x));
	if (Real_negative_p(x))
		m = s9_bignum_negate(m);
	return m;
}

/*
 * Remove trailing zeros and move the decimal
 * point to the END of the mantissa, e.g.:
 * real_normalize(1.234e0) --> 1234e-3
 *
 * Limit the mantissa to S9_MANTISSA_SEGMENTS
 * machine words. This may cause a loss of
 * precision.
 *
 * Also handle numeric overflow/underflow.
 */

static cell real_normalize(cell x) {
	cell	m, e, r;
	int	dgs;

	prot(x);
	e = Real_exponent(x);
	m = new_atom(T_INTEGER, Real_mantissa(x));
	prot(m);
	dgs = count_digits(cdr(m));
	while (dgs > S9_MANTISSA_SIZE) {
		r = s9_bignum_shift_right(m);
		m = car(r);
		pref(0) = m;
		dgs--;
		e++;
	}
	while (!bignum_zero_p(m)) {
		r = s9_bignum_shift_right(m);
		if (!bignum_zero_p(cdr(r)))
			break;
		m = car(r);
		pref(0) = m;
		e++;
	}
	if (bignum_zero_p(m))
		e = 0;
	r = new_atom(e, NIL);
	if (count_digits(r) > S9_DIGITS_PER_CELL) {
		unprot(2);
		return UNDEFINED;
	}
	r = S9_make_quick_real(Real_flags(x), e, cdr(m));
	unprot(2);
	return r;
}

cell s9_bignum_to_real(cell a) {
	int	e, flags, d;
	cell	m, n;

	prot(a);
	m = s9_flat_copy(a, NULL);
	cadr(m) = labs(cadr(m));
	e = 0;
	if (s9_length(cdr(m)) > S9_MANTISSA_SEGMENTS) {
		d = count_digits(cdr(m));
		while (d > S9_MANTISSA_SIZE) {
			m = s9_bignum_shift_right(m);
			m = car(m);
			e++;
			d--;
		}
	}
	flags = bignum_negative_p(a)? REAL_NEGATIVE: 0;
	n = S9_make_quick_real(flags, e, cdr(m));
	n = real_normalize(n);
	unprot(1);
	return n;
}

cell s9_real_negate(cell a) {
	if (integer_p(a))
		return s9_bignum_negate(a);
	Tmp = a;
	a = Real_negate(a);
	Tmp = NIL;
	return a;
}

cell s9_real_negative_p(cell a) {
	if (integer_p(a))
		return bignum_negative_p(a);
	return Real_negative_p(a);
}

cell s9_real_positive_p(cell a) {
	if (integer_p(a))
		return bignum_positive_p(a);
	return Real_positive_p(a);
}

cell s9_real_zero_p(cell a) {
	if (integer_p(a))
		return bignum_zero_p(a);
	return Real_zero_p(a);
}

cell s9_real_abs(cell a) {
	if (integer_p(a))
		return s9_bignum_abs(a);
	if (Real_negative_p(a)) {
		Tmp = a;
		a = Real_negate(a);
		Tmp = NIL;
		return a;
	}
	return a;
}

/*
 * Scale the number R so that it gets exponent DESIRED_E
 * without changing its value. When there is not enough
 * room for scaling the mantissa of R, return UNDEFINED.
 * E.g.: scale_mantissa(1.0e0, -2, 0) --> 100.0e-2
 *
 * Allow the mantissa to grow to MAX_SIZE segments.
 */

static cell scale_mantissa(cell r, cell desired_e, int max_size) {
	int	dgs;
	cell	n, e;

	dgs = count_digits(Real_mantissa(r));
	if (max_size && (max_size - dgs < Real_exponent(r) - desired_e))
		return UNDEFINED;
	n = new_atom(T_INTEGER, s9_flat_copy(Real_mantissa(r), NULL));
	prot(n);
	e = Real_exponent(r);
	while (e > desired_e) {
		n = s9_bignum_shift_left(n, 0);
		pref(0) = n;
		e--;
	}
	unprot(1);
	return S9_make_quick_real(Real_flags(r), e, cdr(n));
}

static void autoscale(cell *pa, cell *pb) {
	if (Real_exponent(*pa) < Real_exponent(*pb)) {
		*pb = scale_mantissa(*pb, Real_exponent(*pa),
					S9_MANTISSA_SIZE*2);
		return;
	}
	if (Real_exponent(*pa) > Real_exponent(*pb)) {
		*pa = scale_mantissa(*pa, Real_exponent(*pb),
					S9_MANTISSA_SIZE*2);
	}
}

cell shift_mantissa(cell m) {    
	m = new_atom(T_INTEGER, m);
	prot(m);
	m = s9_bignum_shift_right(m);
	unprot(1);
	return cdar(m);
}

static int real_compare(cell a, cell b, int approx) {
	cell	ma, mb, d, e;
	int	p;

	if (integer_p(a) && integer_p(b))
		return s9_bignum_equal_p(a, b);
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(a)) {
		a = s9_bignum_to_real(a);
		pref(1) = a;
	}
	if (integer_p(b)) {
		prot(a);
		b = s9_bignum_to_real(b);
		unprot(1);
		pref(0) = b;
	}
	if (Real_zero_p(a) && Real_zero_p(b)) {
		unprot(2);
		return 1;
	}
	if (Real_negative_p(a) != Real_negative_p(b)) {
		unprot(2);
		return 0;
	}
	if (approx) {
		d = s9_real_abs(s9_real_subtract(a, b));
		/* integer magnitudes */
		ma = count_digits(Real_mantissa(a))+Real_exponent(a);
		mb = count_digits(Real_mantissa(b))+Real_exponent(b);
		if (ma != mb) {
			unprot(2);
			return 0;
		}
		p = ma-S9_MANTISSA_SIZE;
		prot(d);
		e = S9_make_quick_real(0, p, cdr(One));
		unprot(3);
		return !s9_real_less_p(e, d);
	}
	unprot(2);
	if (Real_exponent(a) != Real_exponent(b))
		return 0;
	ma = Real_mantissa(a);
	mb = Real_mantissa(b);
	while (ma != NIL && mb != NIL) {
		if (car(ma) != car(mb))
			return 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	if (ma != mb)
		return 0;
	return 1;
}

int s9_real_equal_p(cell a, cell b) {
	return real_compare(a, b, 0);
}

int s9_real_approx_p(cell a, cell b) {
	return real_compare(a, b, 1);
}

int s9_real_less_p(cell a, cell b) {
	cell	ma, mb;
	int	ka, kb, neg;
	int	dpa, dpb;

	if (integer_p(a) && integer_p(b))
		return s9_bignum_less_p(a, b);
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(a))
		a = s9_bignum_to_real(a);
	if (integer_p(b)) {
		prot(a);
		b = s9_bignum_to_real(b);
		unprot(1);
	}
	unprot(2);
	if (Real_negative_p(a) && !Real_negative_p(b)) return 1;
	if (Real_negative_p(b) && !Real_negative_p(a)) return 0;
	if (Real_zero_p(a) && Real_positive_p(b)) return 1;
	if (Real_zero_p(b) && Real_positive_p(a)) return 0;
	neg = Real_negative_p(a);
	dpa = count_digits(Real_mantissa(a)) + Real_exponent(a);
	dpb = count_digits(Real_mantissa(b)) + Real_exponent(b);
	if (dpa < dpb) return neg? 0: 1;
	if (dpa > dpb) return neg? 1: 0;
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	autoscale(&a, &b);
	unprot(2);
	if (a == UNDEFINED) return neg? 1: 0;
	if (b == UNDEFINED) return neg? 0: 1;
	ma = Real_mantissa(a);
	mb = Real_mantissa(b);
	ka = s9_length(ma);
	kb = s9_length(mb);
	if (ka < kb) return 1;
	if (ka > kb) return 0;
	while (ma != NIL) {
		if (car(ma) < car(mb)) return neg? 0: 1;
		if (car(ma) > car(mb)) return neg? 1: 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	return 0;
}

cell s9_real_add(cell a, cell b) {
	cell	r, m, e, aa, ab;
	int	flags, nega, negb;

	if (integer_p(a) && integer_p(b))
		return s9_bignum_add(a, b);
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(a))
		a = s9_bignum_to_real(a);
	prot(a);
	if (integer_p(b))
		b = s9_bignum_to_real(b);
	prot(b);
	if (Real_zero_p(a)) {
		unprot(4);
		return b;
	}
	if (Real_zero_p(b)) {
		unprot(4);
		return a;
	}
	autoscale(&a, &b);
	if (a == UNDEFINED || b == UNDEFINED) {
		ab = s9_real_abs(pref(0));
		prot(ab);
		aa = s9_real_abs(pref(2));
		unprot(1);
		b = unprot(1);
		a = unprot(1);
		unprot(2);
		return s9_real_less_p(aa, ab)? b: a;
	}
	pref(1) = a;
	pref(0) = b;
	e = Real_exponent(a);
	nega = Real_negative_p(a);
	negb = Real_negative_p(b);
	a = new_atom(T_INTEGER, Real_mantissa(a));
	if (nega)
		a = s9_bignum_negate(a);
	pref(1) = a;
	b = new_atom(T_INTEGER, Real_mantissa(b));
	if (negb)
		b = s9_bignum_negate(b);
	pref(0) = b;
	m = s9_bignum_add(a, b);
	flags = bignum_negative_p(m)? REAL_NEGATIVE: 0;
	r = s9_bignum_abs(m);
	r = S9_make_quick_real(flags, e, cdr(r));
	r = real_normalize(r);
	unprot(4);
	return r;
}

cell s9_real_subtract(cell a, cell b) {
	cell	r;

	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(b))
		b = s9_bignum_negate(b);
	else
		b = Real_negate(b);
	prot(b);
	r = s9_real_add(a, b);
	unprot(3);
	return r;
}

cell s9_real_multiply(cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg;

	if (integer_p(a) && integer_p(b))
		return s9_bignum_multiply(a, b);
	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(a))
		a = s9_bignum_to_real(a);
	prot(a);
	if (integer_p(b))
		b = s9_bignum_to_real(b);
	prot(b);
	neg = Real_negative_flag(a) != Real_negative_flag(b);
	ea = Real_exponent(a);
	eb = Real_exponent(b);
	ma = new_atom(T_INTEGER, Real_mantissa(a));
	pref(1) = ma;
	mb = new_atom(T_INTEGER, Real_mantissa(b));
	pref(0) = mb;
	e = ea + eb;
	m = s9_bignum_multiply(ma, mb);
	r = S9_make_quick_real(neg? REAL_NEGATIVE: 0, e, cdr(m));
	r = real_normalize(r);
	unprot(4);
	return r;
}

cell s9_real_divide(cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg, div2;
	int	nd, dd;

	Tmp = b;
	prot(a);
	prot(b);
	Tmp = NIL;
	if (integer_p(a))
		a = s9_bignum_to_real(a);
	prot(a);
	if (integer_p(b))
		b = s9_bignum_to_real(b);
	prot(b);
	if (Real_zero_p(b)) {
		unprot(4);
		return UNDEFINED;
	}
	if (Real_zero_p(a)) {
		r = S9_make_quick_real(0, 0, cdr(Zero));
		unprot(4);
		return r;
	}
	neg = Real_negative_flag(a) != Real_negative_flag(b);
	ea = Real_exponent(a);
	eb = Real_exponent(b);
	ma = new_atom(T_INTEGER, Real_mantissa(a));
	pref(1) = ma;
	mb = new_atom(T_INTEGER, Real_mantissa(b));
	pref(0) = mb;
	if (bignum_zero_p(mb)) {
		unprot(4);
		return UNDEFINED;
	}
	nd = count_digits(cdr(ma));
	dd = S9_MANTISSA_SIZE + count_digits(cdr(mb));
	while (nd < dd) {
		ma = s9_bignum_shift_left(ma, 0);
		pref(1) = ma;
		nd++;
		ea--;
	}
	e = ea - eb;
	m = s9_bignum_divide(ma, mb);
	prot(m);
	div2 = s9_bignum_abs(mb);
	div2 = s9_bignum_divide(div2, Two);
	div2 = car(div2);
	if (s9_bignum_less_p(div2, cdr(m))) {
		m = s9_bignum_add(car(m), One);
	}
	else {
		m = car(m);
	}
	r = S9_make_quick_real(neg? REAL_NEGATIVE: 0, e, cdr(m));
	r = real_normalize(r);
	unprot(5);
	return r;
}

cell s9_real_sqrt(cell x) {
	cell	n0, n1;
	int	r;

	if (s9_real_negative_p(x))
		return UNDEFINED;
	if (s9_real_zero_p(x))
		return Zero;
	prot(x);
	n0 = x;
	prot(n0);
	while (1) {
		n1 = s9_real_divide(x, n0);
		if (n1 == UNDEFINED)
			break;
		n1 = s9_real_add(n1, n0);
		n1 = s9_real_divide(n1, Two);
		prot(n1);
		r = s9_real_approx_p(n0, n1);
		n0 = unprot(1);
		if (r) {
			break;
		}
		pref(0) = n0;
	}
	unprot(2);
	return n1;
}

/*
 * Real power algorithm from
 * http://stackoverflow.com/questions/3518973
 * Thanks, Tom Sirgedas!
 */

static cell rpower(cell x, cell y, cell prec) {
	cell	n, nprec;

	if (Abort_flag)
		return Zero;
	if (s9_real_equal_p(y, One))
		return x;
	if (!s9_real_less_p(y, Ten)) {
		prot(x);
		n = s9_real_divide(y, Two);
		pref(0) = n;
		nprec = s9_real_divide(prec, Two);
		prot(nprec);
		n = rpower(x, n, nprec);
		if (n == UNDEFINED || Abort_flag) {
			unprot(2);
			return UNDEFINED;
		}
		unprot(1);
		pref(0) = n;
		n = s9_real_multiply(n, n);
		unprot(1);
		return n;
	}
	if (!s9_real_less_p(y, One)) {
		y = s9_real_subtract(y, One);
		prot(y);
		n = rpower(x, y, prec);
		if (n == UNDEFINED || Abort_flag) {
			unprot(1);
			return UNDEFINED;
		}
		unprot(1);
		n = s9_real_multiply(x, n);
		return n;
	}
	if (!s9_real_less_p(prec, One))
		return s9_real_sqrt(x);
	y = s9_real_multiply(y, Two);
	prot(y);
	nprec = s9_real_multiply(prec, Two);
	prot(nprec);
	n = rpower(x, y, nprec);
	if (n == UNDEFINED || Abort_flag) {
		unprot(2);
		return UNDEFINED;
	}
	unprot(2);
	return s9_real_sqrt(n);
}

static cell npower(cell x, cell y) {
	cell	n;
	int	even;

	if (Abort_flag)
		return Zero;
	if (s9_real_zero_p(y))
		return One;
	if (s9_real_equal_p(y, One))
		return x; 
	prot(x);
	n = s9_bignum_divide(y, Two);
	even = bignum_zero_p(cdr(n));
	pref(0) = n;
	n = npower(x, car(n));
	if (Abort_flag) {
		unprot(1);
		return Zero;
	}
	pref(0) = n;
	n = s9_real_multiply(n, n);
	pref(0) = n;
	if (!even) {
		n = s9_real_multiply(x, n);
		pref(0) = n;
	}
	unprot(1);
	return n;
}

cell s9_real_power(cell x, cell y) {
	Tmp = x;
	prot(y);
	prot(x);
	Tmp = NIL;
	if (integer_p(y)) {
		x = npower(x, y);
		if (bignum_negative_p(y))
			x = s9_real_divide(One, x);
		unprot(2);
		return x;
	}
	if (s9_real_negative_p(y)) {
		y = s9_real_abs(y);
		prot(y);
		x = rpower(x, y, Epsilon);
		unprot(3);
		if (x == UNDEFINED)
			return x;
		return s9_real_divide(One, x);
	}
	x = rpower(x, y, Epsilon);
	unprot(2);
	return x;
}

/* type: 0=trunc, 1=floor, 2=ceil */
static cell rround(cell x, int type) {
	cell	n, m, e;

	e = s9_real_exponent(x);
	if (e >= 0)
		return x;
	prot(x);
	m = new_atom(T_INTEGER, Real_mantissa(x));
	prot(m);
	while (e < 0) {
		m = s9_bignum_shift_right(m);
		m = car(m);
		pref(0) = m;
		e++;
	}
	if (	(type == 1 && Real_negative_p(x)) ||
		(type == 2 && Real_positive_p(x))
	) {
		m = s9_bignum_add(m, One);
	}
	n = S9_make_real(Real_flags(x), e, cdr(m));
	unprot(2);
	return n;
}

cell s9_real_trunc(cell x) { return rround(x, 0); }
cell s9_real_floor(cell x) { return rround(x, 1); }
cell s9_real_ceil (cell x) { return rround(x, 2); }

cell s9_real_to_bignum(cell r) {
	cell	n;
	int	neg;

	if (Real_exponent(r) >= 0) {
		prot(r);
		neg = Real_negative_p(r);
		n = scale_mantissa(r, 0, 0);
		if (n == UNDEFINED) {
			unprot(1);
			return UNDEFINED;
		}
		n = new_atom(T_INTEGER, Real_mantissa(n));
		if (neg)
			n = s9_bignum_negate(n);
		unprot(1);
		return n;
	}
	return UNDEFINED;
}

cell s9_real_integer_p(cell x) {
	if (integer_p(x))
		return 1;
	if (real_p(x) && s9_real_to_bignum(x) != UNDEFINED)
		return 1;
	return 0;
}

/*
 * String/number conversion
 */

static int exponent_char_p(int c) {
	return c && strchr(Exponent_chars, c) != NULL;
}

int s9_integer_string_p(char *s) {
	if (*s == '-' || *s == '+')
		s++;
	if (!*s)
		return 0;
	while (isdigit(*s))
		s++;
	return *s == 0;
}

int s9_string_numeric_p(char *s) {
	int	i;
	int	got_point = 0,
		got_digit = 0;

	i = 0;
	if (s[0] == '+' || s[0] == '-')
		i = 1;
	if (!s[i])
		return 0;
	while (s[i]) {
		if (isdigit(s[i])) {
			got_digit = 1;
			i++;
		}
		else if (s[i] == '.' && !got_point) {
			got_point = 1;
			i++;
		}
		else {
			break;
		}
	}
	if (!got_digit)
		return 0;
	if (s[i] && strchr(Exponent_chars, s[i]))
		return s9_integer_string_p(&s[i+1]);
	return s[i] == 0;
}

cell s9_string_to_bignum(char *s) {
	cell	n, v, str;
	int	k, j, sign;

	sign = 1;
	if (s[0] == '-') {
		s++;
		sign = -1;
	}
	else if (s[0] == '+') {
		s++;
	}
	str = s9_make_string(s, strlen(s));
	prot(str);
	s = string(str);
	k = (int) strlen(s);
	n = NIL;
	while (k) {
		j = k <= S9_DIGITS_PER_CELL? k: S9_DIGITS_PER_CELL;
		v = s9_asctol(&s[k-j]);
		s[k-j] = 0;
		k -= j;
		n = new_atom(v, n);
		s = string(str);
	}
	unprot(1);
	car(n) = sign * car(n);
	return new_atom(T_INTEGER, n);
}

cell s9_string_to_real(char *s) {
	cell	mantissa, n;
	cell	exponent;
	int	found_dp;
	int	neg = 0;
	int	i, j, v;

	mantissa = Zero;
	prot(mantissa);
	exponent = 0;
	i = 0;
	if (s[i] == '+') {
		i++;
	}
	else if (s[i] == '-') {
		neg = 1;
		i++;
	}
	found_dp = 0;
	while (isdigit((int) s[i]) || s[i] == '#' || s[i] == '.') {
		if (s[i] == '.') {
			i++;
			found_dp = 1;
			continue;
		}
		if (found_dp)
			exponent--;
		mantissa = s9_bignum_shift_left(mantissa, 0);
		pref(0) = mantissa;
		if (s[i] == '#')
			v = 5;
		else
			v = s[i]-'0';
		mantissa = s9_bignum_add(mantissa, s9_make_integer(v));
		pref(0) = mantissa;
		i++;
	}
	j = 0;
	for (n = cdr(mantissa); n != NIL; n = cdr(n))
		j++;
	if (exponent_char_p(s[i])) {
		i++;
		if (!isdigit(s[i]) && s[i] != '-' && s[i] != '+') {
			unprot(1);
			return UNDEFINED;
		}
		n = s9_string_to_bignum(&s[i]);
		if (cddr(n) != NIL) {
			unprot(1);
			return UNDEFINED;
		}
		exponent += cadr(n);
	}
	unprot(1);
	n = S9_make_quick_real((neg? REAL_NEGATIVE: 0),
			exponent, cdr(mantissa));
	return real_normalize(n);
}

cell s9_string_to_number(char *s) {
	if (s9_integer_string_p(s))
		return s9_string_to_bignum(s);
	else
		return s9_string_to_real(s);
}

void s9_print_bignum(cell n) {
	int	first;
	char	buf[S9_DIGITS_PER_CELL+2];

	n = cdr(n);
	first = 1;
	while (n != NIL) {
		s9_prints(ntoa(buf, car(n), first? 0: S9_DIGITS_PER_CELL));
		n = cdr(n);
		first = 0;
	}
}

void s9_print_expanded_real(cell n) {
	char	buf[S9_DIGITS_PER_CELL+3];
	int	k, first;
	int	dp_offset, old_offset;
	cell	m, e;
	int	n_digits, neg;

	m = Real_mantissa(n);
	e = Real_exponent(n);
	neg = Real_negative_p(n);
	n_digits = count_digits(m);
	dp_offset = e+n_digits;
	if (neg)
		s9_prints("-");
	if (dp_offset <= 0)
		s9_prints("0");
	if (dp_offset < 0)
		s9_prints(".");
	while (dp_offset < 0) {
		s9_prints("0");
		dp_offset++;
	}
	dp_offset = e+n_digits;
	first = 1;
	while (m != NIL) {
		ntoa(buf, labs(car(m)), first? 0: S9_DIGITS_PER_CELL);
		k = strlen(buf);
		old_offset = dp_offset;
		dp_offset -= k;
		if (dp_offset < 0 && old_offset >= 0) {
			memmove(&buf[k+dp_offset+1], &buf[k+dp_offset],
				-dp_offset+1);
			buf[k+dp_offset] = '.';
		}
		s9_prints(buf);
		m = cdr(m);
		first = 0;
	}
	if (dp_offset >= 0) {
		while (dp_offset > 0) {
			s9_prints("0");
			dp_offset--;
		}
		s9_prints(".0");
	}
}

void s9_print_sci_real(cell n) {
	int	n_digits;
	cell	m, e;
	char	buf[S9_DIGITS_PER_CELL+2];
	char	es[2];

	m = Real_mantissa(n);
	e = Real_exponent(n);
	n_digits = count_digits(m);
	if (Real_negative_flag(n))
		s9_prints("-");
	ntoa(buf, car(m), 0);
	s9_blockwrite(buf, 1);
	s9_prints(".");
	s9_prints(buf[1] || cdr(m) != NIL? &buf[1]: "0");
	m = cdr(m);
	while (m != NIL) {
		s9_prints(ntoa(buf, car(m), S9_DIGITS_PER_CELL));
		m = cdr(m);
	}
	es[0] = Exponent_chars[0];
	es[1] = 0;
	s9_prints(es);
	if (e+n_digits-1 >= 0)
		s9_prints("+");
	s9_prints(ntoa(buf, e+n_digits-1, 0));
}

void s9_print_real(cell n) {
	int	n_digits;
	cell	m, e;

	m = Real_mantissa(n);
	e = Real_exponent(n);
	n_digits = count_digits(m);
	if (e+n_digits > -S9_MANTISSA_SIZE && e+n_digits <= S9_MANTISSA_SIZE) {
		s9_print_expanded_real(n);
		return;
	}
	s9_print_sci_real(n);
}

cell s9_bignum_to_int(cell x, int *of) {
	int	a, b, s;

	*of = 0;
	if (small_int_p(x)) return small_int_value(x);
	if (NIL == cdddr(x)) {
		if ((size_t) S9_INT_SEG_LIMIT > (size_t) INT_MAX)
			s9_fatal("bignum_to_int(): multi-segment integers "
				"unsupported in 64-bit mode");
		a = cadr(x);
		b = caddr(x);
		if (a > INT_MAX / S9_INT_SEG_LIMIT) {
			*of = 1;
			return 0;
		}
		if (a < INT_MIN / S9_INT_SEG_LIMIT) {
			*of = 1;
			return 0;
		}
		s = a<0? -1: 1;
		a = abs(a) * S9_INT_SEG_LIMIT;
		if (b > INT_MAX - a) {
			*of = 1;
			return 0;
		}
		return s*(a+b);
	}
	*of = 1;
	return 0;
}

cell s9_int_to_bignum(int v) {
	cell	n;

	if (v >= 0 && (long) v < S9_INT_SEG_LIMIT)
		return s9_make_integer(v);
	if (v < 0 && (long) -v < S9_INT_SEG_LIMIT)
		return s9_make_integer(v);
	if ((size_t) S9_INT_SEG_LIMIT > (size_t) INT_MAX)
		s9_fatal("int_to_bignum(): multi-segment integers "
			"unsupported in 64-bit mode");
	n = new_atom(abs(v) % S9_INT_SEG_LIMIT, NIL);
	n = new_atom(v / S9_INT_SEG_LIMIT, n);
	return new_atom(T_INTEGER, n);
}

cell s9_bignum_to_string(cell x) {
	int	n;
	cell	s;
	int	ioe;

	prot(x);
	n = count_digits(cdr(x));
	if (bignum_negative_p(x))
		n++;
	s = s9_make_string("", n);
	Str_outport = string(s);
	Str_outport_len = n+1;
	ioe = IO_error;
	IO_error = 0;
	s9_print_bignum(x);
	n = IO_error;
	IO_error = ioe;
	Str_outport = NULL;
	Str_outport_len = 0;
	unprot(1);
	if (n) {
		return UNDEFINED;
	}
	return s;
}

cell s9_real_to_string(cell x, int mode) {
	#define Z S9_MANTISSA_SIZE+S9_DIGITS_PER_CELL+10
	char	buf[Z];
	int	ioe, n;

	Str_outport = buf;
	Str_outport_len = Z;
	ioe = IO_error;
	IO_error = 0;
	switch (mode) {
	case 0:	s9_print_real(x); break;
	case 1:	s9_print_sci_real(x); break;
	case 2:	s9_print_expanded_real(x); break;
	default:
		Str_outport = NULL;
		Str_outport_len = 0;
		return UNDEFINED;
		break;
	}
	Str_outport = NULL;
	Str_outport_len = 0;
	n = IO_error;
	IO_error = ioe;
	if (n) {
		return UNDEFINED;
	}
	return s9_make_string(buf, strlen(buf));
}

/*
 * I/O
 */

void s9_close_port(int port) {
	if (port < 0 || port >= S9_MAX_PORTS)
		return;
	if (Ports[port] == NULL) {
		Port_flags[port] = 0;
		return;
	}
	fclose(Ports[port]); /* already closed? don't care */
	Ports[port] = NULL;
	Port_flags[port] = 0;
}

int s9_new_port(void) {
	int	i, tries;

	for (tries=0; tries<2; tries++) {
		for (i=0; i<S9_MAX_PORTS; i++) {
			if (Ports[i] == NULL)
				return i;
		}
		if (tries == 0)
			s9_gc();
	}
	return -1;
}

int s9_open_input_port(char *path) {
	int	i = s9_new_port();

	if (i < 0)
		return -1;
	Ports[i] = fopen(path, "r");
	if (Ports[i] == NULL)
		return -1;
	return i;
}

int s9_open_output_port(char *path, int append) {
	int	i = s9_new_port();

	if (i < 0)
		return -1;
	Ports[i] = fopen(path, append? "a": "w");
	if (Ports[i] == NULL)
		return -1;
	return i;
}

int s9_port_eof(int p) {
	if (p < 0 || p >= S9_MAX_PORTS)
		return -1;
	return feof(Ports[p]);
}

int s9_error_port(void) {
	return Error_port;
}

int s9_input_port(void) {
	return Str_inport? -1: Input_port;
}

int s9_output_port(void) {
	return Output_port;
}

cell s9_set_input_port(cell port) {
	cell	p = Input_port;

	Input_port = port;
	return p;
}

cell s9_set_output_port(cell port) {
	cell	p = Output_port;

	Output_port = port;
	return p;
}

void s9_reset_std_ports(void) {
	clearerr(stdin);
	clearerr(stdout);
	clearerr(stderr);
	Input_port = 0;
	Output_port = 1;
	Error_port = 2;
}

int s9_lock_port(int port) {
	if (port < 0 || port >= S9_MAX_PORTS)
		return -1;
	Port_flags[port] |= S9_LOCK_TAG;
	return 0;
}

int s9_unlock_port(int port) {
	if (port < 0 || port >= S9_MAX_PORTS)
		return -1;
	Port_flags[port] &= ~S9_LOCK_TAG;
	return 0;
}

/*
 * Primitives
 */

static char *expected(int n, cell who, char *what) {
	static char	msg[100];
	S9_PRIM		*p;

	p = &Primitives[cadr(who)];
	sprintf(msg, "%s: expected %s in argument #%d", p->name, what, n);
	return msg;
}

static char *wrongargs(char *name, char *what) {
	static char	buf[100];

	sprintf(buf, "%s: too %s arguments", name, what);
	return buf;
}

char *s9_typecheck(cell f) {
	S9_PRIM	*p;
	int	na, i, k;

	k = narg();
	p = prim_info(f);
	if (k < p->min_args)
		return wrongargs(p->name, "few");
	if (k > p->max_args && p->max_args >= 0)
		return wrongargs(p->name, "many");
	na = p->max_args < 0? p->min_args: p->max_args;
	if (na > k)
		na = k;
	else if (na > 3)
		na = 3;
	for (i=1; i<=na; i++) {
		switch (p->arg_types[i-1]) {
		case T_ANY:
			break;
		case T_BOOLEAN:
			if (!boolean_p(parg(i)))
				return expected(i, f, "boolean");
			break;
		case T_CHAR:
			if (!char_p(parg(i)))
				return expected(i, f, "char");
			break;
		case T_INPUT_PORT:
			if (!input_port_p(parg(i)))
				return expected(i, f, "input-port");
			break;
		case T_INTEGER:
			if (!integer_p(parg(i)))
				return expected(i, f, "integer");
			break;
		case T_OUTPUT_PORT:
			if (!output_port_p(parg(i)))
				return expected(i, f, "output-port");
			break;
		case T_PAIR:
			if (atom_p(parg(i)))
				return expected(i, f, "pair");
			break;
		case T_LIST:
			if (parg(i) != NIL && atom_p(parg(i)))
				return expected(i, f, "list");
			break;
		case T_FUNCTION:
			if (	!function_p(parg(i)) &&
				!primitive_p(parg(i)) &&
				!continuation_p(parg(i))
			)
				return expected(i, f, "function");
			break;
		case T_REAL:
			if (!integer_p(parg(i)) && !real_p(parg(i)))
				return expected(i, f, "number");
			break;
		case T_STRING:
			if (!string_p(parg(i)))
				return expected(i, f, "string");
			break;
		case T_SYMBOL:
			if (!symbol_p(parg(i)))
				return expected(i, f, "symbol");
			break;
		case T_VECTOR:
			if (!vector_p(parg(i)))
				return expected(i, f, "vector");
			break;
		}
	}
	return NULL;
}

cell s9_apply_prim(cell f) {
	S9_PRIM	*p;

	p = prim_info(f);
	return (*p->handler)();
}

/*
 * Image I/O
 */

struct magic {
	char	id[16];			/* "magic#"	*/
	char	version[8];		/* "yyyymmdd"	*/
	char	cell_size[1];		/* size + '0'	*/
	char    mantissa_size[1];	/* size + '0'	*/
	char	byte_order[8];		/* e.g. "4321"	*/
	char	prim_slots[8];		/* see code	*/
	char	pad[6];
};

static char *xfwrite(void *buf, int siz, int n, FILE *f) {
	if (fwrite(buf, siz, n, f) != n) {
		return "image file write error";
	}
	return NULL;
}

char *s9_dump_image(char *path, char *magic) {
	FILE		*f;
	cell		n, **v;
	int		i;
	struct magic	m;
	char		*s;

	f = fopen(path, "wb");
	if (f == NULL) {
		return "cannot create image file";
	}
	memset(&m, '_', sizeof(m));
	strncpy(m.id, magic, sizeof(m.id));
	strncpy(m.version, S9_VERSION, sizeof(m.version));
	m.cell_size[0] = sizeof(cell)+'0';
	m.mantissa_size[0] = S9_MANTISSA_SEGMENTS+'0';
#ifdef S9_BITS_PER_WORD_64
	n = 0x3132333435363738L;
#else
	n = 0x31323334L;
#endif
	memcpy(m.byte_order, &n, sizeof(n)>8? 8: sizeof(n));
	n = Last_prim;
	memcpy(m.prim_slots, &n, sizeof(n)>8? 8: sizeof(n));
	if ((s = xfwrite(&m, sizeof(m), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	i = Cons_pool_size;
	if ((s = xfwrite(&i, sizeof(int), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	i = Vec_pool_size;
	if ((s = xfwrite(&i, sizeof(int), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	if (	(s = xfwrite(&Free_list, sizeof(cell), 1, f)) != NULL ||
		(s = xfwrite(&Free_vecs, sizeof(cell), 1, f)) != NULL ||
		(s = xfwrite(&Symbols, sizeof(cell), 1, f)) != NULL ||
		(s = xfwrite(&Symhash, sizeof(cell), 1, f)) != NULL
	) {
		fclose(f);
		return s;
	}
	i = 0;
	v = Image_vars;
	while (v[i]) {
		if ((s = xfwrite(v[i], sizeof(cell), 1, f)) != NULL) {
			fclose(f);
			return s;
		}
		i++;
	}
	if (	fwrite(Car, 1, Cons_pool_size*sizeof(cell), f)
		 != Cons_pool_size*sizeof(cell) ||
		fwrite(Cdr, 1, Cons_pool_size*sizeof(cell), f)
		 != Cons_pool_size*sizeof(cell) ||
		fwrite(Tag, 1, Cons_pool_size, f) != Cons_pool_size ||
		fwrite(Vectors, 1, Vec_pool_size*sizeof(cell), f)
		 != Vec_pool_size*sizeof(cell)
	) {
		fclose(f);
		return "image dump failed";
	}
	fclose(f);
	return NULL;
}

static char *xfread(void *buf, int siz, int n, FILE *f) {
	if (fread(buf, siz, n, f) != n) {
		return "image file read error";
	}
	return NULL;
}

char *s9_load_image(char *path, char *magic) {
	FILE		*f;
	cell		n, **v;
	int		i;
	struct magic	m;
	int		image_nodes, image_vcells;
	char		*s;

	f = fopen(path, "rb");
	if (f == NULL)
		return "could not open file";
	if ((s = xfread(&m, sizeof(m), 1, f)) != NULL)
		return s;
	if (memcmp(m.id, magic, 16)) {
		fclose(f);
		return "magic match failed";
	}
	if (memcmp(m.version, S9_VERSION, sizeof(m.version))) {
		fclose(f);
		return "wrong image version";
	}
	if (m.cell_size[0]-'0' != sizeof(cell)) {
		fclose(f);
		return "wrong cell size";
	}
	if (m.mantissa_size[0]-'0' != S9_MANTISSA_SEGMENTS) {
		fclose(f);
		return "wrong mantissa size";
	}
	memcpy(&n, m.byte_order, sizeof(cell));
#ifdef S9_BITS_PER_WORD_64
	if (n != 0x3132333435363738L) {
#else
	if (n != 0x31323334L) {
#endif
		fclose(f);
		return "wrong byte order";
	}
	memcpy(&n, m.prim_slots, sizeof(cell));
	if (n != Last_prim) {
		fclose(f);
		return "wrong number of primitives";
	}
	memset(Tag, 0, Cons_pool_size);
	if ((s = xfread(&image_nodes, sizeof(int), 1, f)) != NULL)
		return s;
	if ((s = xfread(&image_vcells, sizeof(int), 1, f)) != NULL)
		return s;
	while (image_nodes > Cons_pool_size) {
		if (	Node_limit &&
			Cons_pool_size + Cons_segment_size > Node_limit
		) {
			fclose(f);
			return "image cons pool too large";
		}
		new_cons_segment();
	}
	while (image_vcells > Vec_pool_size) {
		if (	Vector_limit &&
			Vec_pool_size + Vec_segment_size > Vector_limit
		) {
			fclose(f);
			return "image vector pool too large";
		}
		new_vec_segment();
	}
	if (	(s = xfread(&Free_list, sizeof(cell), 1, f)) != NULL ||
		(s = xfread(&Free_vecs, sizeof(cell), 1, f)) != NULL ||
		(s = xfread(&Symbols, sizeof(cell), 1, f)) != NULL ||
		(s = xfread(&Symhash, sizeof(cell), 1, f)) != NULL
	) {
		fclose(f);
		return s;
	}
	v = Image_vars;
	i = 0;
	while (v[i]) {
		if ((s = xfread(v[i], sizeof(cell), 1, f)) != NULL)
			return s;
		i++;
	}
	if (	(fread(Car, 1, image_nodes*sizeof(cell), f)
		  != image_nodes*sizeof(cell) ||
		 fread(Cdr, 1, image_nodes*sizeof(cell), f)
		  != image_nodes*sizeof(cell) ||
		 fread(Tag, 1, image_nodes, f) != image_nodes ||
		 fread(Vectors, 1, image_vcells*sizeof(cell), f)
		  != image_vcells*sizeof(cell) ||
		 fgetc(f) != EOF)
	) {
		fclose(f);
		return "wrong file size";
	}
	fclose(f);
	return NULL;
}

/*
 * Initialization
 */

void s9_exponent_chars(char *s) {
	Exponent_chars = s;
}

void s9_image_vars(cell **v) {
	Image_vars = v;
}

void s9_add_image_vars(cell **v) {
	int	i, n, m;
	cell	**nv;

	if (Image_vars != NULL) {
		for (n=0; Image_vars[n] != NULL; n++)
			;
		for (m=0; v[m] != NULL; m++)
			;
		nv = malloc((n+m+1) * sizeof(cell *));
		if (nv == NULL)
			s9_fatal("add_image_vars(): out of memory");
		n = 0;
		for (i = 0; Image_vars[i] != NULL; i++)
			nv[n++] = Image_vars[i];
		for (i = 0; v[i] != NULL; i++)
			nv[n++] = v[i];
		nv[n] = NULL;
		v = nv;
	}
	Image_vars = v;
}

static void resetpools(void) {
	Cons_segment_size = S9_INITIAL_SEGMENT_SIZE;
	Vec_segment_size = S9_INITIAL_SEGMENT_SIZE;
	Cons_pool_size = 0,
	Vec_pool_size = 0;
	Car = NULL,
	Cdr = NULL;
	Tag = NULL;
	Free_list = NIL;
	Vectors = NULL;
	Free_vecs = 0;
	Primitives = NULL;
	Max_prims = 0;
}

void s9_init(cell **extroots, cell *stack, int *stkptr) {
	int	i;

#ifdef S9_BITS_PER_WORD_64
	if (sizeof(cell) != 8)
		s9_fatal("improper 64-bit build");
#endif
	GC_ext_roots = extroots;
	GC_stack = stack;
	GC_stkptr = stkptr;
	for (i=2; i<S9_MAX_PORTS; i++)
		Ports[i] = NULL;
	Ports[0] = stdin;
	Ports[1] = stdout;
	Ports[2] = stderr;
	Port_flags[0] = S9_LOCK_TAG;
	Port_flags[1] = S9_LOCK_TAG;
	Port_flags[2] = S9_LOCK_TAG;
	Input_port = 0;
	Output_port = 1;
	Error_port = 2;
	Str_outport = NULL;
	Str_outport_len = 0;
	Str_inport = NULL;
	Abort_flag = 0;
	resetpools();
	Node_limit = S9_NODE_LIMIT * 1024L;
	Vector_limit = S9_VECTOR_LIMIT * 1024L;
	Stack = NIL,
	Tmp_car = NIL;
	Tmp_cdr = NIL;
	Tmp = NIL;
	Symbols = NIL;
	Symhash = NIL;
	Printer_limit = 0;
	IO_error = 0;
	Exponent_chars = "eE";
	Run_stats = 0;
	Cons_stats = 0;
	Mem_error_handler = NULL;
	new_cons_segment();
	new_vec_segment();
	s9_gc();
	Protect = s9_make_vector(PROT_STACK_LEN);
	Protp = -1;
	Zero = make_init_integer(0);
	One = make_init_integer(1);
	Two = make_init_integer(2);
	Ten = make_init_integer(10);
	Nullvec = s9_new_vec(T_VECTOR, 0);
	Nullstr = s9_new_vec(T_STRING, 1);
	Blank = new_atom(T_CHAR, new_atom(' ', NIL));
	Epsilon = S9_make_quick_real(0, -S9_MANTISSA_SIZE, cdr(One));
}

void s9_fini() {
	int	i;

	for (i=2; i<S9_MAX_PORTS; i++) {
		if (Ports[i] != NULL)
			fclose(Ports[i]);
		Ports[i] = NULL;
	}
	if (Car) free(Car);
	if (Cdr) free(Cdr);
	if (Tag) free(Tag);
	if (Vectors) free(Vectors);
	if (Primitives) free(Primitives);
	resetpools();
}

/***********************************************************************
			Test cases below
***********************************************************************/

#ifdef TEST
volatile int	Mem_err = 0;
int		Errors = 0;

#define TESTFILE	"__testfile__"

void fail(char *s) {
	printf("Failed: %s\n", s);
	Errors++;
}

cell pcons(void) {
	if (narg() != 2) fail("apply_prim(0)");
	return cons(parg(1), parg(2));
}

cell	A, B, N;

cell	R;
int	P;

cell	*Roots[] = { &A, &B, &N, &R, NULL };

S9_PRIM Pr = { "cons", pcons, 2, 2, { T_ANY, T_LIST, T_ANY } };

void mem_error(int src) {
	Mem_err = src;
}

void test_types(void) {
	cell	n, m;
	int	of;

	if (!eof_p(END_OF_FILE)) fail("eof_p()");
	if (!undefined_p(UNDEFINED)) fail("undefined_p()");
	if (!unspecific_p(UNSPECIFIC)) fail("unspecific_p()");
	if (!s9_special_p(USER_SPECIALS)) fail("special_p()");
	if (!boolean_p(TRUE)) fail("boolean_p(TRUE)");
	if (!boolean_p(FALSE)) fail("boolean_p(FALSE)");
	n = s9_make_char('x');
	if (!char_p(n)) fail("char_p()");
	if (char_value(n) != 'x') fail("char_value()");
	n = s9_make_port(0, T_INPUT_PORT);
	if (!input_port_p(n)) fail("input_port_p()");
	if (port_no(n) != 0) fail("port_no(0)");
	n = s9_make_integer(12345);
	if (!integer_p(n)) fail("integer_p()");
	if (s9_bignum_to_int(n, &of) != 12345) fail("bignum_to_int()");
	n = cons(One, NIL);
	n = cons(Zero, n);
	if (!pair_p(n)) fail("pair_p(1)");
	if (!pair_p(cdr(n))) fail("pair_p(1)");
	if (car(n) != Zero) fail("list/1");
	if (cadr(n) != One) fail("list/2");
	if (cddr(n) != NIL) fail("list/NIL");
	n = s9_make_port(1, T_OUTPUT_PORT);
	if (!output_port_p(n)) fail("output_port_p()");
	if (port_no(n) != 1) fail("port_no(1)");
	n = s9_make_primitive(&Pr);
	if (!primitive_p(n)) fail("primitive_p()");
	if (prim_slot(n) != 0) fail("prim_slot()");
	if (strcmp(prim_info(n)->name, "cons")) fail("prim_info()");
	if (!function_p(new_atom(T_FUNCTION, NIL))) fail("function_p()");
	n = s9_make_real(1, -5, s9_make_integer(12345));
	if (!real_p(n)) fail("real_p()");
	if (s9_real_exponent(n) != -5) fail("real_exponent()");
	if (s9_bignum_to_int(s9_real_mantissa(n), &of) != 12345)
		fail("real_mantissa()");
	n = s9_make_string("hello, world!", 13);
	if (!string_p(n)) fail("string_p()");
	if (strcmp(string(n), "hello, world!")) fail("string()");
	if (string_len(n) != 14) fail("string_len()");
	n = s9_symbol_ref("foobarbaz");
	if (!symbol_p(n)) fail("symbol_p()");
	if (strcmp(symbol_name(n), "foobarbaz")) fail("symbol_name()");
	if (symbol_len(n) != 10) fail("symbol_len()");
	if (s9_symbol_ref("foobarbaz") != n) fail("symbol_ref()");
	if (!syntax_p(new_atom(T_SYNTAX, NIL))) fail("syntax_p()");
	n = s9_make_vector(100);
	vector(n)[0] = Zero;
	vector(n)[99] = One;
	if (!vector_p(n)) fail("vector_p()");
	if (vector(n)[0] != Zero) fail("vector(0)");
	if (vector(n)[99] != One) fail("vector(99)");
	if (vector_len(n) != 100) fail("vector_len()");
	if (!continuation_p(new_atom(T_CONTINUATION, NIL)))
		fail("continuation_p()");
	n = s9_make_string("foo", 3);
	m = s9_copy_string(n);
	if (strcmp(string(n), string(m))) fail("copy_string()");
	if (!atom_p(new_atom(0, NIL))) fail("atom_p()");
	n = s9_new_vec(T_STRING, 100);
	if (!string_p(n)) fail("new_vec(1)");
	if (string_len(n) != 100) fail("new_vec(2)");
	prot(One);
	prot(Two);
	if (pref(0) != Two) fail("pref(1)");
	if (pref(1) != One) fail("pref(2)");
	if (unprot(1) != Two) fail("prot(2)");
	if (unprot(1) != One) fail("prot(1)");
	if (!constant_p(s9_cons3(NIL, NIL, S9_CONST_TAG)))
		fail("constant_p()");
	n = s9_make_primitive(&Pr);
	prot(n);
	m = cons(NIL, NIL); vector(R)[++P] = m;
	m = s9_mkfix(1); vector(R)[++P] = m;
	if (s9_typecheck(n) == NULL) fail("typecheck(1)");
	P = -1;
	m = cons(One, NIL); vector(R)[++P] = m;
	m = cons(Zero, NIL); vector(R)[++P] = m;
	m = s9_mkfix(2); vector(R)[++P] = m;
	if (s9_typecheck(n) == NULL) fail("typecheck(2)");
	P = -1;
	m = cons(NIL, NIL); vector(R)[++P] = m;
	m = cons(Zero, NIL); vector(R)[++P] = m;
	m = s9_mkfix(2); vector(R)[++P] = m;
	if (s9_typecheck(n) != NULL) fail("typecheck(3)");
	n = s9_apply_prim(n);
	unprot(1);
	if (car(n) != Zero) fail("apply_prim(1)");
	if (cdr(n) != NIL) fail("apply_prim(2)");
	if (s9_find_symbol("new-symbol") != NIL) fail("find_symbol(1)");
	n = s9_make_symbol("new-symbol", 10);
	if (s9_find_symbol("new-symbol") != NIL) fail("find_symbol(2)");
	s9_intern_symbol(n);
	if (s9_find_symbol("new-symbol") == NIL) fail("find_symbol(3)");
	m = s9_symbol_to_string(n);
	if (!string_p(m) || strcmp(string(m), "new-symbol"))
		fail("symbol_to_string()");
	if (s9_string_to_symbol(m) != n) fail("string_to_symbol(1)");
	s9_string_to_symbol(s9_make_string("xxyyzz", 6));
	if (s9_find_symbol("xxyyzz") == NIL) fail("string_to_symbol(2)");
	if (s9_type_tag(Zero) != S9_T_INTEGER) fail("type_tag(1)");
	if (s9_type_tag(s9_make_string("", 0)) != S9_T_STRING)
		fail("type_tag(2)");
	if (s9_type_tag(s9_symbol_ref("foo")) != S9_T_SYMBOL)
		fail("type_tag(3)");
	if (s9_type_tag(S9_TRUE) != S9_T_BOOLEAN) fail("type_tag(4)");
	if (s9_type_tag(S9_NIL) != S9_T_NONE) fail("type_tag(5)");
}

void test_fixnum(void) {
	int	n;

	n = 1 << (sizeof(int)*8-2);
	A = s9_mkfix(n);
	if (!s9_fix_p(A)) fail("mkfix(1)");
	if (s9_fixval(A) != n) fail("fixval(1)");
	A = s9_mkfix(-n);
	if (!s9_fix_p(A)) fail("mkfix(2)");
	if (s9_fixval(A) != -n) fail("fixval(2)");
}

void test_bignum(void) {
	cell	n;
	int	of;

#ifndef S9_BITS_PER_WORD_64
	int	v;

	v = INT_MAX;
	A = s9_int_to_bignum(v);
	if (!integer_p(A)) fail("s9_int_to_bignum(1)");
	if (s9_bignum_to_int(A, &of) != v) fail("s9_bignum_to_int(1)");
	A = s9_int_to_bignum(-v);
	if (!integer_p(A)) fail("s9_int_to_bignum(2)");
	if (s9_bignum_to_int(A, &of) != -v) fail("s9_bignum_to_int(2)");
	A = s9_int_to_bignum(INT_MAX);
	A = s9_bignum_add(A, One);
	s9_bignum_to_int(A, &of);
	if (0 == of) fail("s9_bignum_to_int(3)");
	A = s9_string_to_bignum("123456789012345678901234567890");
	s9_bignum_to_int(A, &of);
	if (0 == of) fail("s9_bignum_to_int(4)");
#endif
	if (s9_bignum_to_int(Zero, &of) != 0) fail("Zero");
	if (s9_bignum_to_int(One, &of) != 1) fail("One");
	if (s9_bignum_to_int(Two, &of) != 2) fail("Two");
	n = s9_make_integer(-123);
	if (s9_bignum_to_int(s9_bignum_abs(n), &of) != 123)
		fail("bignum_abs()");
	A = s9_make_integer(1235);
	B = s9_make_integer(5678);
	if (s9_bignum_to_int(s9_bignum_add(A, B), &of) != 6913)
		fail("bignum_add()");
	N = s9_bignum_divide(B, A);
	if (s9_bignum_to_int(car(N), &of) != 4) fail("bignum_divide(1)");
	if (s9_bignum_to_int(cdr(N), &of) != 738) fail("bignum_divide(2)");
	if (s9_bignum_equal_p(A, B)) fail("bignum_equal_p(1)");
	if (!s9_bignum_equal_p(A, A)) fail("bignum_equal_p(2)");
	if (s9_bignum_even_p(A)) fail("bignum_even_p(1)");
	if (!s9_bignum_even_p(B)) fail("bignum_even_p(2)");
	if (!s9_bignum_less_p(A, B)) fail("bignum_less_p(1)");
	if (s9_bignum_less_p(B, A)) fail("bignum_less_p(2)");
	if (s9_bignum_less_p(B, B)) fail("bignum_less_p(3)");
	N = s9_make_integer(123);
	if (s9_bignum_to_int(s9_bignum_multiply(N, N), &of) != 15129)
		fail("bignum_multiply()");
	if (s9_bignum_to_int(s9_bignum_negate(A), &of) != -1235)
		fail("bignum_negate()");
	N = s9_bignum_shift_left(N, 7);
	if (s9_bignum_to_int(N, &of) != 1237) fail("bignum_shift_left()");
	N = s9_bignum_shift_right(A);
	if (s9_bignum_to_int(car(N), &of) != 123) fail("bignum_shift_right(1)");
	if (s9_bignum_to_int(cdr(N), &of) != 5) fail("bignum_shift_right(2)");
	if (s9_bignum_to_int(s9_bignum_subtract(A, B), &of) != -4443)
		fail("bignum_subtract()");
	N = s9_bignum_to_string(A);
	if (!string_p(N) || strcmp(string(N), "1235"))
		fail("bignum_to_string()");
}

cell mant(cell x) {
	int	of;

	return s9_bignum_to_int(s9_real_mantissa(x), &of);
}

cell result(cell r, cell xe, cell xm) {
	int	m = mant(r);

	return xe == s9_real_exponent(r) && m == xm;
}

void test_real(void) {
	int	of;

	if (!real_p(Epsilon)) fail("Epsilon");
	N = s9_make_real(1, 2, s9_make_integer(314));
	if (Real_exponent(N) != 2) fail("Real_exponent()");
	A = s9_bignum_to_int(new_atom(T_INTEGER, Real_mantissa(N)), &of);
	if (A != 314) fail("Real_mantissa()");
	if (Real_negative_flag(N)) fail("Real_negative_flag()");
	if (Real_zero_p(N)) fail("Real_zero_p()");
	if (!Real_positive_p(N)) fail("Real_positive_p()");
	if (Real_negative_p(N)) fail("Real_negative_p(1)");
	N = Real_negate(N);
	if (!Real_negative_p(N)) fail("Real_negative_p(2)");
	A = s9_make_real(1, 1, s9_make_integer(123));
	B = s9_make_real(-1, -1, s9_make_integer(456));
	if (s9_real_negative_p(s9_real_abs(B))) fail("real_abs()");
	N = s9_real_add(A, B);
	if (!result(N, -1, 11844)) fail("real_add()");
	N = s9_real_divide(A, Two);
	if (!result(N, 0, 615)) fail("real_divide()");
	if (s9_real_equal_p(A, B)) fail("real_equal_p(1)");
	if (!s9_real_equal_p(A, A)) fail("real_equal_p(2)");
	N = s9_real_floor(B);
	if (!result(N, 0, -46)) fail("real_floor()");
	N = s9_real_trunc(B);
	if (!result(N, 0, -45)) fail("real_floor()");
	N = s9_real_ceil(B);
	if (!result(N, 0, -45)) fail("real_ceil()");
	if (!s9_real_integer_p(A)) fail("real_integer_p(1)");
	if (s9_real_integer_p(B)) fail("real_integer_p(2)");
	if (s9_real_less_p(A, B)) fail("real_less_p(1)");
	if (!s9_real_less_p(B, A)) fail("real_less_p(1)");
	if (s9_real_less_p(B, B)) fail("real_less_p(2)");
	N = s9_real_multiply(B, Two);
	if (!result(N, -1, -912)) fail("real_multiply()");
	N = s9_real_negate(B);
	if (!result(N, -1, 456)) fail("real_negate()");
	if (s9_real_zero_p(N)) fail("real_zero_p()");
	if (!s9_real_positive_p(N)) fail("real_positive_p()");
	if (s9_real_negative_p(N)) fail("real_negative_p(1)");
	N = s9_real_negate(N);
	if (!s9_real_negative_p(N)) fail("real_negative_p(2)");
	N = s9_real_subtract(A, B);
	if (!result(N, -1, 12756)) fail("real_subtract()");
	N = s9_real_to_bignum(A);
	if (s9_bignum_to_int(N, &of) != 1230) fail("real_to_bignum()");
	N = s9_real_multiply(Ten, Ten);
	N = s9_real_sqrt(N);
	if (!s9_real_equal_p(N, Ten)) fail("real_sqrt(1)");
	N = s9_make_real(1, -2, s9_make_integer(256));
	N = s9_real_sqrt(N);
	A = s9_make_real(1, -1, s9_make_integer(16));
	if (!s9_real_equal_p(N, A)) fail("real_sqrt(2)");
	N = s9_real_power(Two, Ten);
	if (!s9_real_equal_p(N, s9_make_integer(1024))) fail("real_power(1)");
	N = s9_real_power(Two, s9_real_negate(Two));
	A = s9_make_real(1, -2, s9_make_integer(25));
	if (!s9_real_equal_p(N, A)) fail("real_power(2)");
	A = s9_real_sqrt(Two);
	B = s9_make_real(1, -1, s9_make_integer(5));
	B = s9_real_power(Two, B);
	if (!s9_real_equal_p(A, B)) fail("real_power(3)");
	A = s9_make_real(1, -1, s9_make_integer(1));
	B = s9_real_power(Two, A);
}

void print_test(char *name, void (*printer)(cell), cell n, char *s) {
	int	p, op, i;
	char	b[100];

	p = s9_open_output_port(TESTFILE, 0);
	op = s9_output_port();
	s9_set_output_port(p);
	(*printer)(n);
	s9_close_port(p);
	s9_set_output_port(op);
	p = s9_open_input_port(TESTFILE);
	op = s9_input_port();
	s9_set_input_port(p);
	i = s9_blockread(b, 100);
	if (i > 0)
		b[i] = 0;
	s9_close_port(p);
	s9_set_input_port(op);
	if (strcmp(s, b))
		fail(name);
}

void test_io(void) {
	int	c, i, p;
	char	b[100];

	if (s9_input_port() != 0) fail("input_port(1)");
	if (s9_output_port() != 1) fail("output_port(1)");
	p = s9_open_output_port(TESTFILE, 0);
	s9_set_output_port(p);
	s9_prints("0123456789");
	s9_close_port(p);
	s9_reset_std_ports();
	p = s9_open_input_port(TESTFILE);
	s9_set_input_port(p);
	for (i=0; i<5; i++) {
		if ((c = s9_readc()) != "0123456789"[i])
			fail("readc(1)");
	}
	s9_rejectc(c);
	for (i=4; i<10; i++) {
		if ((c = s9_readc()) != "0123456789"[i])
			fail("readc(2)");
	}
	s9_close_port(p);
	s9_reset_std_ports();
	p = s9_open_output_port(TESTFILE, 1);
	s9_set_output_port(p);
	s9_prints("0123456789");
	s9_close_port(p);
	s9_reset_std_ports();
	p = s9_open_input_port(TESTFILE);
	s9_set_input_port(p);
	if (s9_blockread(b, 20) < 20) fail("blockread()");
	s9_close_port(p);
	s9_reset_std_ports();
	if (s9_input_port() != 0) fail("input_port(2)");
	if (s9_output_port() != 1) fail("output_port(2)");
	print_test("print_bignum()", s9_print_bignum,
			s9_string_to_bignum("-12345678901234567890"),
			"-12345678901234567890");
	print_test("print_expanded_real()", s9_print_expanded_real,
			s9_make_real(1, -6, s9_make_integer(12345)),
			"0.012345");
	print_test("print_sci_real()", s9_print_sci_real,
			s9_make_real(1, -6, s9_make_integer(12345)),
			"1.2345e-2");
	for (i=0; i<S9_MAX_PORTS*3; i++)
		if (s9_open_input_port(TESTFILE) < 0)
			fail("port finalization");
}

void test_conv(void) {
	int	of;

	if (s9_integer_string_p("")) fail("s9_integer_string_p(1)");
	if (s9_integer_string_p("-")) fail("s9_integer_string_p(2)");
	if (!s9_integer_string_p("0")) fail("s9_integer_string_p(3)");
	if (!s9_integer_string_p("-1")) fail("s9_integer_string_p(4)");
	if (!s9_integer_string_p("+01234567890"))
		fail("s9_integer_string_p(5)");
	if (s9_string_numeric_p("")) fail("s9_string_numeric_p(1)");
	if (s9_string_numeric_p("-")) fail("s9_string_numeric_p(2)");
	if (!s9_string_numeric_p("0")) fail("s9_string_numeric_p(3)");
	if (!s9_string_numeric_p("-1")) fail("s9_string_numeric_p(4)");
	if (!s9_string_numeric_p("+01234567890")) fail("string_numeric_p(5)");
	if (s9_string_numeric_p(".")) fail("s9_string_numeric_p(6)");
	if (s9_string_numeric_p("0e")) fail("s9_string_numeric_p(7)");
	if (!s9_string_numeric_p(".0")) fail("s9_string_numeric_p(8)");
	if (!s9_string_numeric_p("0.")) fail("s9_string_numeric_p(9)");
	if (!s9_string_numeric_p("1e0")) fail("s9_string_numeric_p(10)");
	if (!s9_string_numeric_p("+1e-23")) fail("s9_string_numeric_p(11)");
	if (!s9_string_numeric_p("-1e+23")) fail("s9_string_numeric_p(12)");
	if (s9_bignum_to_int(s9_string_to_bignum("12345"), &of) != 12345)
		fail("string_to_bignum()");
	N = s9_string_to_real("+12345.6e-7");
	if (!result(N, -8, 123456)) fail("string_to_real()");
}

void test_util(void) {
	int	i;
	cell	a, b;

	if (s9_asctol("12345") != 12345) fail("asctol()");
	N = cons(NIL, NIL);
	for (i=0; i<100; i++)
		N = cons(NIL, N);
	if (s9_length(N) != 101) fail("length()");
	N = s9_flat_copy(N, &a);
	if (s9_length(N) != 101) fail("flat_copy(1)");
	b = cons(NIL, NIL);
	cdr(a) = b;
	if (s9_length(N) != 102) fail("flat_copy(2)");
}

int main(void) {
	s9_init(Roots, &R, &P);
	R = NIL;
	R = s9_make_vector(100);
	P = -1;
	s9_mem_error_handler(mem_error);
	test_types();
	test_fixnum();
	test_bignum();
	test_real();
	test_io();
	test_conv();
	test_util();
	s9_fini();
	remove(TESTFILE);
	bye(Errors);
	return 0;
}
#endif /* TEST */
