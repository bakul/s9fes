/*
 * Scheme 9 from Empty Space, Plan9 Interface
 * By Bakul Shah, 2009-2011,
 *    Nils M Holm, 2015-2016
 * Placed in the Public Domain
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

cell make_ulong_integer(unsigned long long u) {
	cell	n;

	n = new_atom(u % S9_INT_SEG_LIMIT, NIL);
	u /= S9_INT_SEG_LIMIT;
	while (u) {
		n = new_atom(u % S9_INT_SEG_LIMIT, n);
		u /= S9_INT_SEG_LIMIT;
	}
	return new_atom(T_INTEGER, n);
}

cell make_long_integer(long long i) {
	cell		n;

	n = make_ulong_integer(i < 0? -i: i);
	if (i < 0)
		n = new_atom(T_INTEGER, new_atom(-cadr(n), cddr(n)));
	return n;
}

unsigned long long uint64_value(char *src, cell x) {
	unsigned long long	v, ov;
	cell			p;
	cell			seg;
	char			msg[128];

	v = seg = cadr(x);
	if (seg < 0) {
		sprintf(msg, "%s: expected positive value, got", src);
		error(msg, x);
	}
	p = cddr(x);
	while (p != NIL) {
		ov = v;
		v = v * S9_INT_SEG_LIMIT + car(p);
		if ((v - car(p)) / S9_INT_SEG_LIMIT != ov || v < ov) {
			sprintf(msg, "%s: integer too big", src);
			error(msg, x);
		}
		p = cdr(p);
	}
	return v;
}

long long int64_value(char *src, cell x) {
	cell		n;
	long long	v;

	if (cadr(x) < 0)
		n = new_atom(T_INTEGER, new_atom(-cadr(x), cddr(x)));
	else
		n = x;
	v = uint64_value(src, n);
	return cadr(x) < 0? -v: v;
}

unsigned long uint32_value(char *src, cell x) {
	unsigned long long	v = uint64_value(src, x);
	unsigned long		w;
	char			msg[128];

	w = (unsigned long) v;
	if (v != w) {
		sprintf(msg, "%s: uint32 too big", src);
		error(msg, x);
	}
	return w;
}

long int32_value(char *src, cell x) {
	long long	v = int64_value(src, x);
	long		w;
	char		msg[128];

	w = (long) v;
	if (v != w) {
		sprintf(msg, "%s: int32 too big", src);
		error(msg, x);
	}
	return w;
}
