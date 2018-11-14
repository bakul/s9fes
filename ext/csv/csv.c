/*
 * Scheme 9 from Empty Space, CSV Primitives
 * By Nils M Holm, 2017
 * Placed in the Public Domain
 *
 * Primitive functions for processing comma-separated values (CSVs)
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#include <stdlib.h>

int grow(cell n, int i, int k, int kk) {
	cell	m;

	if (i >= k) {
		m = make_string("", k+kk);
		memcpy(string(m), string(car(n)), k);
		k += kk;
		car(n) = m;
	}
	return k;
}

cell pp_csv_read(void) {
	#define	L 256
	cell	n, m;
	int	f = 0, i = 0, k = 0;
	int	c, nc;
	int	ne = 0;

	c = readc();
	if (EOF == c)
		return END_OF_FILE;
	n = cons(NIL, NIL);
	save(n);
	while (c != '\n') {
		if ('"' == c) {
			c = readc();
			if (f && '"' == c) {
				k = grow(n, i, k, L);
				string(car(n))[i++] = c;
				c = readc();
				continue;
			}
			rejectc(c);
			if (0 == f) {
				f = 1;
				ne++;
				car(n) = make_string("", L);
				k = L;
				i = 0;
			}
			else {
				f = 0;
				m = make_string("", i);
				memcpy(string(m), string(car(n)), i+1);
				car(n) = m;
				nc = 0;
				while (' ' == (c = readc()) || ',' == c) {
					if (',' == c && nc++ > 0) {
						cdr(n) = cons(NIL, NIL);
						n = cdr(n);
						m = make_string("", 0);
						car(n) = m;
					}
				}
				if (c != '"')
					break;
				rejectc(c);
				cdr(n) = cons(NIL, NIL);
				n = cdr(n);
			}
		}
		else if (f) {
			k = grow(n, i, k, L);
			string(car(n))[i++] = c;
		}
		c = readc();
	}
	while (c != '\n' && c != EOF)
		c = readc();
	if (0 == ne) {
		unsave(1);
		return NIL;
	}
	if (f) {
		m = make_string("", i);
		memcpy(string(m), string(car(n)), i+1);
		car(n) = m;
	}
	return unsave(1);
}

cell pp_csv_write(void) {
	cell	p;
	char	*s, b[3];

	b[2] = 0;
	for (p = parg(1); p != NIL; p = cdr(p)) {
		if (atom_p(p)) error("csv:write: improper list", parg(1));
		prints("\"");
		b[1] = 0;
		for (s = string(car(p)); *s; s++) {
			b[0] = *s;
			b[1] = '"' == *s? '"': 0;
			prints(b);
		}
		prints("\"");
		if (pair_p(cdr(p)))
			prints(",");
	}
	return UNSPECIFIC;
}

S9_PRIM Csv_primitives[] = {
 { "csv:read",              pp_csv_read,             0,  0, { ___,___,___ } },
 { "csv:write",             pp_csv_write,            1,  1, { LST,___,___ } },
 { NULL }
};

void csv_init(void) {
	add_primitives("csv", Csv_primitives);
}
