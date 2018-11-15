/*
 * Scheme 9 from Empty Space, Plan9 Interface
 * By Bakul Shah, 2009-2011;
 *    Nils M Holm, 2015-2017
 * Placed in the Public Domain
 */

/***********************************************************************

	A low-level interface to some Plan9 system services.

	NOTES:

	To cart around binary data we use string (should use uniform
	byte vector). That means we must do make_string("",len) and
	then memcpy.

	First arg is parg(1) and *not* x!

	Args to a fn are checked for type. But substructure is not.

	Todo:

	+ = some progress
	* = simple case done, general case not yet complete
	? = known to be buggy

	*Handle vlong (int64_t) correctly
	Allow only valid subset of rfork
	fversion
	?dir->msg
	?msg->dir
	*msg->fcall
	+fcall->msg
	*dirread
	profile
	tests

    Bugs:

    Questions:

	Can we avoid conv{D2M,M2D,S2M,M2S}?

	Do global cells need to be saved?
		Yes, with the exception of those created by symbol_ref().

	Can we use rendezvous and semaphores?

	Can we handle notes? [async delivery]

	Can we handle threads?

***********************************************************************/

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"
#include "s9-ffi.h"
#include <fcall.h>

#ifdef length
#undef length
#endif
#ifdef bind
#undef bind
#endif

/*
 *	Allow us at least to write
 *		assign(parg(1), cons(foo, bar));
 *	in presence of that fact that C's
 *	order of evaluation messes up
 *		parg(1) = cons(foo, bar);
 */

static cell	New_node;
#define assign(n,v)	{ New_node = v; n = New_node; }

char	Last_errstr[ERRMAX];
cell	Catch_errors = 0;

static cell
	afid_sym,
	aname_sym,
	aqid_sym,
	atime_sym,
	count_sym,
	data_sym,
	dev_sym,
	ename_sym,
	fid_sym,
	gid_sym,
	length_sym,
	mode_sym,
	msize_sym,
	mtime_sym,
	muid_sym,
	name_sym,
	newfid_sym,
	offset_sym,
	oldtag_sym,
	perm_sym,
	qid_sym,
	stat_sym,
	tag_sym,
	type_sym,
	uid_sym,
	uname_sym,
	wname_sym,
	version_sym;

static cell
	dir_sym,
	Rattach_sym,
	Rauth_sym,
	Rclunk_sym,
	Rcreate_sym,
	Rerror_sym,
	Rflush_sym,
	Ropen_sym,
	Rread_sym,
	Rremove_sym,
	Rstat_sym,
	Rversion_sym,
	Rwalk_sym,
	Rwrite_sym,
	Rwstat_sym,
	Tattach_sym,
	Tauth_sym,
	Tclunk_sym,
	Tcreate_sym,
	Tflush_sym,
	Topen_sym,
	Tread_sym,
	Tremove_sym,
	Tstat_sym,
	Tversion_sym,
	Twalk_sym,
	Twrite_sym,
	Twstat_sym;

cell *Plan9_image_vars[] = {
	&afid_sym, &aqid_sym, &aname_sym, &atime_sym, &count_sym,
	&data_sym, &dev_sym, &ename_sym, &fid_sym, &gid_sym, &length_sym,
	&mode_sym, &msize_sym, &mtime_sym, &muid_sym, &name_sym,
	&newfid_sym, &offset_sym, &oldtag_sym, &perm_sym, &qid_sym,
	&stat_sym, &type_sym, &uid_sym, &uname_sym, &wname_sym, &dir_sym,
	&Rattach_sym, &Rauth_sym, &Rclunk_sym, &Rcreate_sym, &Rerror_sym,
	&Rflush_sym, &Ropen_sym, &Rread_sym, &Rremove_sym, &Rstat_sym,
	&Rversion_sym, &Rwalk_sym, &Rwrite_sym, &Rwstat_sym, &Tattach_sym,
	&Tauth_sym, &Tclunk_sym, &Tcreate_sym, &Tflush_sym, &Topen_sym,
	&Tread_sym, &Tremove_sym, &Tstat_sym, &Tversion_sym, &Twalk_sym,
	&Twrite_sym, &Twstat_sym,
NULL };

enum {
	STATSIZE = 200//STATFIXLEN + 16 * 4
};

inline static cell cstr2string(char*str) {
	return make_string(str, strlen(str));
}

static int str2qid(char* str, Qid* qid) {
	int	i, r;

	r = sscanf(str, "%Lx:%lx:%x", &qid->path, &qid->vers, &i);
	if (r != 3)
		return -1;
	qid->type = i;
	return 0;
}

static cell qid2string(Qid* qid) {
      char	b[30];

      int l = sprint(b, "%016llx:%08lx:%02x", qid->path, qid->vers, qid->type);
      return make_string(b, l);
}

static char* string2str(cell val, char** s, char*) {
	int	len;
	char*	buf;

	if (!string_p(val))
		return 0;
	len = string_len(val);
	buf = *s;
	strncpy(buf, string(val), len);
	*s += len + 1;
	buf[len] = 0;
	return buf;
}

cell sys_convM2D(uchar* edir, int len) {
	uchar	tmp[300];
	Dir	*d = (Dir*)tmp;
	cell	n, new;
	int	i;

	convM2D(edir, len, d, (char*)(d+1));
	n = make_vector(12);
	for (i = 0; i < 12; i++)
		vector(n)[i] = NIL;
	save(n);
	i = 0;
	new = dir_sym;                      vector(n)[i++] = new;
	new = make_long_integer(d->type);   vector(n)[i++] = new;
	new = make_long_integer(d->dev);    vector(n)[i++] = new;
	new = qid2string(&d->qid);          vector(n)[i++] = new;
	new = make_long_integer(d->mode);   vector(n)[i++] = new;
	new = make_long_integer(d->atime);  vector(n)[i++] = new;
	new = make_long_integer(d->mtime);  vector(n)[i++] = new;
	new = make_long_integer(d->length); vector(n)[i++] = new;
	new = cstr2string(d->name);         vector(n)[i++] = new;
	new = cstr2string(d->uid);          vector(n)[i++] = new;
	new = cstr2string(d->gid);          vector(n)[i++] = new;
	new = cstr2string(d->muid);         vector(n)[i] = new;
	unsave(1);
	return n;
}

int sys_convD2M(cell x, uchar* buf, int len) {
	char	name[] = "sys:convD2M";
	char	tmp[300]; // XXX arbitrary?
	Dir	*d;
	int	r;
	char	*b, *e;
	int	i;

	d = (Dir*)tmp;
	b = tmp + sizeof *d;
	e = tmp + sizeof tmp;
	memset(d, 0, sizeof *d);
	if (vector(x)[0] != dir_sym)
		return -1;
	i = 1;
	if (!integer_p(vector(x)[i]))
		return -1;
	d->type = integer_value(name, vector(x)[i++]);
	if (!integer_p(vector(x)[i]))
		return -1;
	d->dev = int32_value(name, vector(x)[i++]);
	if (	!string_p(vector(x)[i]) ||
		str2qid(string(vector(x)[i++]), &d->qid)
	)
		return -1;
	if (!integer_p(vector(x)[i]))
		return -1;
	d->mode = uint32_value(name, vector(x)[i++]);
	if (!integer_p(vector(x)[i]))
		return -1;
	d->atime = uint32_value(name, vector(x)[i++]);
	if (!integer_p(vector(x)[i]))
		return -1;
	d->mtime = uint32_value(name, vector(x)[i++]);
	if (!integer_p(vector(x)[i]))
		return -1;
	d->length = uint64_value(name, vector(x)[i++]);
	if (!(d->name = string2str(vector(x)[i++], &b, e)))
		return -1;
	if (!(d->uid = string2str(vector(x)[i++], &b, e)))
		return -1;
	if (!(d->gid = string2str(vector(x)[i++], &b, e)))
		return -1;
	if (!(d->muid = string2str(vector(x)[i], &b, e)))
		return -1;
	r = sizeD2M(d);
	if (r > len)
		return -1;
	convD2M(d, buf, r);
	return r;
}

cell sys_convM2S(uchar* edir, int len) {
	int	i;
	uchar	tmp[200];
	Fcall	*f = (Fcall*) tmp;
	cell	n, *v;

	if (!convM2S(edir, len, f))
	    return NIL;

	n = make_vector(6);
	v = vector(n);
	for (i = 0; i < 6; i++)
		v[i] = NIL;

	save(n);
	v[1] = make_long_integer(f->tag);

	switch (f->type) {
	default:
		/* convM2S will take care of this */
		return 0;
	case Tversion:
		v[0] = Tversion_sym;
		v[2] = make_ulong_integer(f->msize);
		v[3] = cstr2string(f->version);
		break;
	case Tauth:
		v[0] = Tauth_sym;
		v[2] = make_ulong_integer(f->afid);
		v[3] = cstr2string(f->uname);
		v[4] = cstr2string(f->aname);
		break;
	case Tflush:
		v[0] = Tflush_sym;
		v[2] = make_ulong_integer(f->oldtag);
		break;
	case Tattach:
		v[0] = Tattach_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = make_ulong_integer(f->afid);
		v[4] = cstr2string(f->uname);
		v[5] = cstr2string(f->aname);
		break;
	case Twalk:
		v[0] = Twalk_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = make_ulong_integer(f->newfid);
		v[4] = new_vec(T_VECTOR, f->nwname * sizeof(cell));
		v = vector(v[4]);
		for (i = 0; i < f->nwname; i++)
			v[i] = cstr2string(f->wname[i]);
		break;
	case Topen:
		v[0] = Topen_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = make_ulong_integer(f->mode);
		break;
	case Tcreate:
		v[0] = Tcreate_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = cstr2string(f->name);
		v[4] = make_ulong_integer(f->perm);
		v[5] = make_ulong_integer(f->mode);
		break;
	case Tread:
		v[0] = Tread_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = make_long_integer(f->offset);
		v[4] = make_long_integer(f->count);
		break;
	case Twrite:
		v[0] = Twrite_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = make_long_integer(f->offset);
		v[4] = make_string(f->data, f->count);
		break;
	case Tclunk:
		v[2] = make_ulong_integer(f->fid);
		break;
	case Tremove:
		v[2] = make_ulong_integer(f->fid);
		break;
	case Tstat:
		v[2] = make_ulong_integer(f->fid);
		break;
	case Twstat:
		v[2] = make_ulong_integer(f->fid);
		v[3] = sys_convM2D(f->stat, f->nstat);
		break;

	case Rversion:
		v[0] = Rversion_sym;
		v[2] = make_ulong_integer(f->msize);
		v[3] = cstr2string(f->version);
		break;
	case Rauth:
		v[0] = Rauth_sym;
		v[2] = qid2string(&f->aqid);
		break;
	case Rerror:
		v[0] = Rerror_sym;
		v[2] = cstr2string(f->ename);
		break;
	case Rflush:
		v[0] = Rflush_sym;
		break;
	case Rattach:
		v[0] = Rattach_sym;
		v[2] = qid2string(&f->qid);
		break;
	case Rwalk:
		v[0] = Rwalk_sym;
		v[2] = new_vec(T_VECTOR, f->nwqid*sizeof(cell));
		v = vector(v[2]);
		for (i = 0; i < f->nwqid; i++)
			v[i] = NIL;
		for (i = 0; i < f->nwqid; i++)
			v[i] = qid2string(&f->wqid[i]);
		break;
	case Ropen:
		v[0] = Ropen_sym;
		v[2] = qid2string(&f->qid);
		v[3] = make_ulong_integer(f->iounit);
		break;
	case Rcreate:
		v[0] = Rcreate_sym;
		v[2] = qid2string(&f->qid);
		v[3] = make_ulong_integer(f->iounit);
		break;
	case Rread:
		v[0] = Rread_sym;
		v[2] = make_ulong_integer(f->count);
		v[3] = make_string(f->data, f->count);
		break;
	case Rwrite:
		v[0] = Rwrite_sym;
		v[2] = make_ulong_integer(f->count);
		break;
	case Rclunk:
		v[0] = Rclunk_sym;
		break;
	case Rremove:
		v[0] = Rclunk_sym;
		break;
	case Rstat:
		v[0] = Rstat_sym;
		v[2] = make_ulong_integer(f->fid);
		v[3] = sys_convM2D(f->stat, f->nstat);
		break;
	case Rwstat:
		v[0] = Rwstat_sym;
		break;
	}
	unsave(1);
	return n;
}

int sys_convS2M(cell x, uchar*buf, int len) {
	Fcall	*f = (Fcall*)&buf;
	int	r, flen, i;
	char	*b, *e;
	cell	*v;
	
	b = (char*)&f[1];
	e = (char*)buf + len;
	v = vector(x);
	i = 2;
	f->tag = make_ulong_integer(v[1]);
	if (v[0] == Tversion_sym) {
		f->type = Tversion;
		f->version = (char*)b;
		flen = string_len(v[i]);
		strncpy(f->version, string(v[i]), flen);
		f->version[flen++] = 0;
		b += flen; i++;
		f->msize = make_ulong_integer(v[i]);
	} else if (v[0] == Tauth_sym) {
		f->type = Tauth;
		f->afid = make_ulong_integer(v[i++]);
		if (!(f->uname = string2str(v[i++], &b, e)))
			return -1;
		if (!(f->aname = string2str(v[i], &b, e)))
			return -1;
	} else if (v[0] == Tattach_sym) {
		f->type = Tattach;
		f->fid = make_ulong_integer(v[i++]);
		f->afid = make_ulong_integer(v[i++]);
		if (!(f->uname = string2str(v[i++], &b, e)))
			return -1;
		if (!(f->aname = string2str(v[i], &b, e)))
			return -1;
	} else if (v[0] == Tflush_sym) {
		f->type = Tflush;
		f->oldtag = make_ulong_integer(v[i]);
	} else if (v[0] == Twalk_sym) {
		int j;
		f->type = Twalk;
		f->fid = make_ulong_integer(v[i++]);
		f->newfid = make_ulong_integer(v[i++]);
		/* XXX check vector */
		f->nwname = vector_len(v[i]);
		v = vector(v[i]);
		for (j = 0; j < f->nwname; j++)
			if (!(f->wname[j] = string2str(v[j], &b, e)))
				return -1;
	} else if (v[0] == Topen_sym) {
		f->type = Topen;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Tcreate_sym) {
		f->type = Tcreate;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Tread_sym) {
		f->type = Tread;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Twrite_sym) {
		f->type = Twrite;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Tclunk_sym) {
		f->type = Tclunk;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Tremove_sym) {
		f->type = Tremove;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Tstat_sym) {
		f->type = Tstat;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Twstat_sym) {
		f->type = Twstat;
		f->fid = make_ulong_integer(v[i]);

	} else if (v[0] == Rversion_sym) {
		f->type = Rversion;
		f->msize = make_ulong_integer(v[i++]);
		if (!(f->version = string2str(v[i], &b, e)))
			return -1;
	} else if (v[0] == Rauth_sym) {
		f->type = Rauth;
	} else if (v[0] == Rattach_sym) {
		f->type = Rattach;
	} else if (v[0] == Rerror_sym) {
		f->type = Rerror;
	} else if (v[0] == Rflush_sym) {
		f->type = Rflush;
	} else if (v[0] == Rwalk_sym) {
		f->type = Rwalk;
	} else if (v[0] == Ropen_sym) {
		f->type = Ropen;
	} else if (v[0] == Rcreate_sym) {
		f->type = Rcreate;
	} else if (v[0] == Rread_sym) {
		f->type = Rread;
	} else if (v[0] == Rwrite_sym) {
		f->type = Rwrite;
		f->fid = make_ulong_integer(v[i]);
	} else if (v[0] == Rclunk_sym) {
		f->type = Rclunk;
	} else if (v[0] == Rremove_sym) {
		f->type = Rremove;
	} else if (v[0] == Rstat_sym) {
		f->type = Rstat;
	} else if (v[0] == Rwstat_sym) {
		f->type = Rwstat;
	} else
		return -1;
	r = sizeS2M(f);
	if (r > len)
		return -1;
	convS2M(f, buf, len);
	return r;
}

cell sys_error(char *who) {
	char	buf[ERRMAX+20];

	errstr(Last_errstr, sizeof Last_errstr);
	if (who) {
		int i, k;
		cell a;
		char *p, *q = Last_errstr;
		if (Catch_errors) return FALSE;
		k = strlen(who);
		strcpy(buf, who);
		strcpy(&buf[k], ": ");
		k += 2;
		for (p = buf+k; *q && k < 244; k++)
			*p++ = *q++;
		*p = 0;
		strcat(p, ", arguments");
		save(a = NIL);
		for (i = narg(); i > 0; i--) {
			a = cons(parg(i), a);
			car(Stack) = a;
		}
		unsave(1);
		error(buf, a);
	}
	return FALSE;
}

cell sys_ok(void) {
	return Catch_errors? TRUE: UNSPECIFIC;
}

cell pp_sys_alarm(void) {
	if (alarm(integer_value("sys:alarm", parg(1))))
		return sys_error("sys:alarm");
	return sys_ok();
}

cell pp_sys_await(void) {
	char buf[ERRMAX+40];
	int len =  await(buf, sizeof buf - 1);
	if (len <= 0)
		return FALSE;
	return make_string(buf, len);
}

cell pp_sys_bind(void) {
	return bind(string(parg(1)), string(parg(2)), 
		integer_value("sys:access", parg(3))) < 0? FALSE: TRUE;
}

#ifdef BRK_
/* XXX -- not sure how useful this is .... */
cell pp_sys_brk(void) {
	if (brk((void*)integer_value("sys:brk", parg(1))) < 0)
		return sys_error("sys:brk_");
	return sys_ok();
}

/* XXX -- not sure how useful this is .... */
cell pp_sys_sbrk(void) {
	uintptr bl; 
	bl = sbrk(integer_value("sys:sbrk", parg(1)));
	if (bl == (void*)-1)
		return sys_error("sys:sbrk");
	return make_long_integer(bl);
}
#endif

cell pp_sys_catch_errors(void) {
	Catch_errors = parg(1) == TRUE;
	if (Catch_errors) Last_errstr[0] = '\0';
	return UNSPECIFIC;
}

cell pp_sys_chdir(void) {
	if (chdir(string(parg(1))) < 0)
		return sys_error("sys:chdir");
	return sys_ok();
}

cell pp_sys_close(void) {
	if (close(integer_value("sys:close", parg(1))) < 0)
		return sys_error("sys:close");
	return sys_ok();
}

cell pp_sys_convD2M(void) {
	uchar	buf[STATSIZE];
	int	len = sys_convD2M(parg(1), buf, STATSIZE);
	cell	n;

	/* XXX check size */
	if (len < 0)
		return FALSE;
	n = make_string("", len);
	memcpy(string(n), (char *) buf, len);
	return n;
}

cell pp_sys_convM2D(void) {
	uchar*	buf = (uchar*)string(parg(1));
	int	len = string_len(parg(1));

	/* XXX check size */
	if (len < 0)
		return FALSE;
	return sys_convM2D(buf, BIT16SZ + GBIT16(buf));
}

cell pp_sys_convS2M(void) {
	uchar	buf[STATSIZE];
	int	len = sys_convS2M(parg(1), buf, STATSIZE);

	/* XXX check size */
	if (len < 0)
		return FALSE;
	return make_string((char*)buf, len);
}

cell pp_sys_convM2S(void) {
	uchar*	buf = (uchar*)string(parg(1));
	int	len = string_len(parg(1));

	/* XXX check size */
	if (len < 0)
		return FALSE;
	return sys_convM2S(buf, BIT16SZ + GBIT16(buf));
}

cell pp_sys_create(void) {
	int	fd;
	char	name[] = "sys:create";

	fd = create(string(parg(1)),
		    integer_value(name, parg(2)),
		    integer_value(name, parg(3)));
	if (fd < 0)
		return sys_error(name);
	return make_long_integer(fd);
}

cell pp_sys_dirread(void) {
	cell	r, a;
	char	name[] = "sys:dirread";
	int	fd = integer_value(name, parg(1));
	cell	b = make_string("", DIRMAX);
	uchar*	buf = (uchar*)string(b);
	int	i, m, c;

	c = read(fd, buf, DIRMAX);
	if (c < 0)
		return sys_error(name);
	save(b);
	r = cons(NIL, NIL);
	save(r);
	a = r;

	for (i = 0; i < c; i += m) {
		m = BIT16SZ + GBIT16(buf+i);
		if (statcheck(buf+i, m) < 0) {
			unsave(2);
			return sys_error(name);
		}
		assign(car(a), sys_convM2D(buf+i, m));
		assign(cdr(a), cons(NIL, NIL));
		a = cdr(a);
	}
	unsave(2);
	return r;
}

cell pp_sys_dup(void) {
	int	r;
	char	name[] = "sys:dup";

	r = dup(integer_value(name, parg(1)),
		integer_value(name, parg(2)));
	if (r < 0)
		return sys_error(name);
	return make_long_integer(r);
}

cell pp_sys_errstr(void) {
	int	len;
	char	*buf1 = string(parg(1));
	char	buf[ERRMAX];

	strcpy(buf, buf1);
	len =  errstr(buf, ERRMAX);
	if (len < 0)
		return sys_error("sys:errstr");
	return make_string(buf, len);
}

cell pp_sys_exec(void) {
	char	**argv;
	cell	p;
	int	i;

	for (p = parg(2); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			error("sys:exec: improper list, last element is",
				p);
		if (!string_p(car(p)))
			error("sys:exec: expected list of string, got",
				car(p));
	}
	argv = malloc((s9_length(parg(2)) + 2) * sizeof(char *));
	if (argv == NULL)
		sys_error("sys:exec");
	argv[0] = string(parg(1));
	i = 1;
	for (p = parg(2); p != NIL; p = cdr(p))
		argv[i++] = string(car(p));
	argv[i] = NULL;
	exec(string(parg(1)), argv);
	return sys_error("sys:exec");
}

cell pp_sys_exits(void) {
	exits(string(parg(1)));
	fatal("exits() failed");
	return sys_ok();
}

cell pp_sys_fauth(void) {
	int	r;
	char	name[] = "sys:fauth";

	r = fauth(integer_value(name, parg(1)), string(parg(2)));
	if (r < 0)
		return sys_error(name);
	return make_long_integer(r);
}

cell pp_sys_fd2path(void) {
	int	len;
	char	name[] = "sys:fd->path";

	cell buf = make_string("", 1024), buf2;
	if (fd2path(integer_value(name, parg(1)), string(buf), 1024))
		return sys_error(name);
	len = strlen(string(buf));
	save(buf);
	buf2 = make_string("", len);
	unsave(1);
	strcpy(string(buf2), string(buf));
	return buf2;
}

cell pp_sys_fork(void) {
	int	pid;

	pid = fork();
	if (pid < 0)
		return sys_error("sys:fork");
	return make_long_integer(pid);
}

cell pp_sys_fstat(void) {
	uchar	edir[STATSIZE];
	char	name[]="sys:fstat";
	int	len = fstat(integer_value(name, parg(1)), edir, STATSIZE);

	if (len < 0)
		return sys_error(name);
	return make_string((char*)edir, len);
}

cell pp_sys_fwstat(void) {
	cell	fd = parg(1);
	cell	st = parg(2);
	int	r;

	r = fwstat(integer_value("sys:fwstat", fd),
		   (uchar*)string(st), string_len(st));
	return r < 0? FALSE : TRUE;
}

cell pp_sys_mount(void) {
	char	name[] = "sys:mount";
	cell	y = parg(4);
	cell	z = parg(5);

	if (!integer_p(y))
		error("sys:mount: expected integer, got", y);
	if (!string_p(z))
		error("sys:mount: expected string, got", z);
	return mount(integer_value(name, parg(1)),
		     integer_value(name, parg(2)), 
		     string(parg(3)), 
		     integer_value(name, y), 
		     string(z)) < 0 ? FALSE : TRUE;
}

cell pp_sys_open(void) {
	int	fd;

	fd = open(string(parg(1)), integer_value("sys:open", parg(2)));
	if (fd < 0)
		return sys_error("sys:open");
	return make_long_integer(fd);
}

cell pp_sys_pipe(void) {
	int	fd[2];
	cell	n;

	if (pipe(fd) < 0)
		return sys_error("sys:pipe");
	n = cons(make_long_integer(fd[1]), NIL);
	save(n);
	n = cons(make_long_integer(fd[0]), n);
	unsave(1);
	return n;
}

cell pp_sys_postnote(void) {
	char	name[] = "sys:postnote";
	int	r;

	r = postnote(integer_value(name, parg(1)), 
		     integer_value(name, parg(2)),
		     string(parg(2)));
	if (r < 0)
		return sys_error(name);
	return TRUE;
}

cell pp_sys_pread(void) {
	cell	buf, buf2;
	int	r, k;
	char	name[] = "sys:pread";

	k = integer_value(name, parg(2));
	buf = make_string("", k);
	r = pread(integer_value(name, parg(1)), string(buf), k,
		  int64_value(name, parg(3)));
	if (r < 0)
		return sys_error(name);
	if (r < k) {
		save(buf);
		buf2 = make_string("", r);
		unsave(1);
		memcpy(string(buf2), string(buf), r);
		buf = buf2;
	}
	return buf;
}

cell pp_sys_pwrite(void) {
	int	r;
	char	name[] = "sys:pwrite";

	r = pwrite(integer_value(name, parg(1)), string(parg(2)),
		string_len(parg(2))-1, int64_value(name, parg(3)));
	if (r < 0)
		return sys_error(name);
	return make_long_integer(r);
}

cell pp_sys_read(void) {
	cell	buf, buf2;
	int	r, k;
	char	name[] = "sys:read";

	k = integer_value(name, parg(2));
	buf = make_string("", k);
	r = read(integer_value(name, parg(1)), string(buf), k);
	if (r < 0)
		return sys_error(name);
	{
	int i;
	for (i = 0; i < 0; i++) print("%d ", string(buf)[i]);
	}
	if (r < k) {
		save(buf);
		buf2 = make_string("", r);
		unsave(1);
		memcpy(string(buf2), string(buf), r);
		buf = buf2;
	}
	return buf;
}

cell pp_sys_rendezvous(void) {
	char*	r;

	r = rendezvous(symbol_name(parg(1)), string(parg(2)));
	if (r == (char*)-1)
		return sys_error("sys:rendezvous");
	return make_string(r, strlen(r));
}

cell pp_sys_rfork(void) {
	int	pid;

	pid = rfork(integer_value("sys:rfork", parg(1)));
	if (pid < 0)
		return sys_error("sys:rfork");
	return make_long_integer(pid);
}

cell pp_sys_remove(void) {
	if (remove(string(parg(1))) < 0)
		return sys_error("sys:remove");
	return sys_ok();
}

cell pp_sys_seek(void) {
	char	name[] = "sys:seek";
	vlong	r;

	r = seek(integer_value(name, parg(1)),
		int64_value(name, parg(2)),
		integer_value(name, parg(3)));
	if (r < 0LL)
		return sys_error("sys:seek");
	return make_long_integer(r);
}

cell pp_sys_sleep(void) {
	if (sleep(integer_value("sys:sleep", parg(1))))
		return sys_error("sys:sleep");
	return sys_ok();
}

cell pp_sys_stat(void) {
	uchar	edir[STATSIZE];
	int	len = stat(string(parg(1)), edir, STATSIZE);
	cell	r;
	if (len < 0)
		return sys_error("sys:stat");
	r = make_string("", len);
	memcpy(string(r), edir, len);
	return r;
}

cell pp_sys_unmount(void) {
	return unmount(string(parg(1)), string(parg(2))) < 0? FALSE : TRUE;
}

cell pp_sys_wait(void) {
	cell	n;
	char	buf[ERRMAX+40];
	int	len =  await(buf, sizeof buf - 1);
	char*	fld[5];

	if (len < 0)
		return sys_error("sys:wait");
	tokenize(buf, fld, nelem(fld));
	n = cons(make_string(fld[4], strlen(fld[4])), NIL);
	save(n);
	n = cons(make_long_integer(atoi(fld[3])), n);
	n = cons(make_long_integer(atoi(fld[2])), n);
	n = cons(make_long_integer(atoi(fld[1])), n);
	n = cons(make_long_integer(atoi(fld[0])), n);
	unsave(1);
	return n;
}

cell pp_sys_waitpid(void) {
	char	buf[ERRMAX+40];
	int	len =  await(buf, sizeof buf - 1);
	char*	fld[5];

	if (len < 0)
		return sys_error("sys:wait");
	tokenize(buf, fld, nelem(fld));
	return make_long_integer(atoi(fld[0]));
}

cell pp_sys_write(void) {
	int	r;

	r = write(integer_value("sys:write", parg(1)), string(parg(2)),
		string_len(parg(2))-1);
	if (r < 0)
		return sys_error("sys:write");
	return make_long_integer(r);
}

cell pp_sys_wstat(void) {
	uchar*	buf = (uchar*)string(parg(2));
	int	len = string_len(parg(2));
	int	r;

	r = wstat(string(parg(1)), buf, len);
	return r < 0? FALSE : TRUE;
}

cell pp_sys_command_line(void) {
	extern cell	Argv;

	return Argv;
}

#define	K(x)	{#x, (int)x}
struct Magic_const {
	char*	name;
	int	value;
};

typedef struct Magic_const Magic_const;
static Magic_const magic_const[] = {
	K(DMDIR),
	K(DMMOUNT),
	K(DMAUTH),
	K(DMAPPEND),
	K(DMEXCL),
	K(DMEXEC),
	K(DMREAD),
	K(DMTMP),
	K(DMWRITE),

	K(MORDER),
	K(MREPL),
	K(MBEFORE),
	K(MAFTER),
	K(MCREATE),
	K(MCACHE),

	K(STATMAX),
	K(DIRMAX),
	K(ERRMAX),

	K(OREAD),
	K(OWRITE),
	K(ORDWR),
	K(OEXEC),
	K(OTRUNC),
	K(OCEXEC),
	K(ORCLOSE),
	K(OEXCL),

	K(NCONT),
	K(NDFLT),
	K(NSAVE),
	K(NRSTR),

	K(QTDIR),
	K(QTAPPEND),
	K(QTEXCL),
	K(QTMOUNT),
	K(QTAUTH),
	K(QTTMP),
	K(QTFILE),

	K(RFNAMEG),
	K(RFENVG),
	K(RFFDG),
	K(RFNOTEG),
	K(RFPROC),
	K(RFMEM),
	K(RFNOWAIT),
	K(RFCNAMEG),
	K(RFCENVG),
	K(RFCFDG),
	K(RFREND),
	K(RFNOMNT),

	K(PNPROC),
	K(PNGROUP),
	{0,0}
};

cell pp_sys_magic_const(void) {
	char*		name = string(parg(1));
	Magic_const	*k;

	for (k = magic_const; k->name; k++)
		if (strcmp(k->name, name) == 0)
			return make_long_integer(k->value);
	return FALSE;
}

S9_PRIM Plan9_primitives[] = {
 {"sys:alarm",      pp_sys_alarm,      1, 1, { INT,___,___ } },
 {"sys:await",      pp_sys_await,      0, 0, { ___,___,___ } },
 {"sys:bind",       pp_sys_bind,       3, 3, { STR,STR,INT } },
#ifdef _BRK
 {"sys:brk",        pp_sys_brk,        1, 1, { INT,___,___ } },
#endif
 {"sys:catch-errors",pp_sys_catch_errors,1,1,{ BOL,___,___ } },
 {"sys:chdir",      pp_sys_chdir,      1, 1, { STR,___,___ } },
 {"sys:close",      pp_sys_close,      1, 1, { INT,___,___ } },
 {"sys:create",     pp_sys_create,     3, 3, { STR,INT,INT } },
 {"sys:convd2m",    pp_sys_convD2M,    1, 1, { VEC,___,___ } },
 {"sys:dirread",    pp_sys_dirread,    1, 1, { INT,___,___ } },
 {"sys:dup",        pp_sys_dup,        2, 2, { INT,INT,___ } },
 {"sys:errstr",     pp_sys_errstr,     1, 1, { STR,___,___ } },
 {"sys:exec",       pp_sys_exec,       2, 2, { STR,LST,___ } },
 {"sys:exits",      pp_sys_exits,      1, 1, { STR,___,___ } },
 {"sys:fauth",      pp_sys_fauth,      0, 0, { ___,___,___ } },
 {"sys:convs2m",    pp_sys_convS2M,    1, 1, { VEC,___,___ } },
 {"sys:fd2path",    pp_sys_fd2path,    1, 1, { INT,___,___ } },
 {"sys:fork",       pp_sys_fork,       0, 0, { ___,___,___ } },
 {"sys:fstat",      pp_sys_fstat,      1, 1, { INT,___,___ } },
#ifdef FOO
 {"sys:fversion",   pp_sys_fversion,   0, 0, { ___,___,___ } },
#endif
 {"sys:fwstat",     pp_sys_fwstat,     2, 2, { INT,STR,___ } },
 {"sys:magic-const",pp_sys_magic_const,1, 1, { STR,___,___ } },
 {"sys:mount",      pp_sys_mount,      5, 5, { INT,INT,STR } },
 {"sys:convm2d",    pp_sys_convM2D,    1, 1, { STR,___,___ } },
 {"sys:convm2s",    pp_sys_convM2S,    1, 1, { STR,___,___ } },
#ifdef FOO
 {"sys:noted",      pp_sys_noted,      1, 1, { INT,___,___ } },
 {"sys:notify",     pp_sys_notify,     1, 1, { STR,___,___ } },
#endif
 {"sys:open",       pp_sys_open,       2, 2, { STR,INT,___ } },
 {"sys:pipe",       pp_sys_pipe,       0, 0, { ___,___,___ } },
 {"sys:postnote",   pp_sys_postnote,   3, 3, { INT,INT,STR } },
 {"sys:pread",      pp_sys_pread,      3, 3, { INT,INT,INT } },
 {"sys:pwrite",     pp_sys_pwrite,     3, 3, { INT,STR,INT } },
 {"sys:read",       pp_sys_read,       2, 2, { INT,INT,___ } },
 {"sys:remove",     pp_sys_remove,     1, 1, { STR,___,___ } },
 {"sys:rendezvous", pp_sys_rendezvous, 2, 2, { SYM,STR,___ } },
 {"sys:rfork",      pp_sys_rfork,      1, 1, { INT,___,___ } },
#ifdef _BRK
 {"sys:sbrk",       pp_sys_sbrk,       1, 1, { INT,___,___ } },
#endif
 {"sys:seek",       pp_sys_seek,       3, 3, { INT,INT,INT } },
#ifdef _BRK
 {"sys:segattach",  pp_sys_segattach,  0, 0, { ___,___,___ } },
 {"sys:segbrk",     pp_sys_segbrk,     0, 0, { ___,___,___ } },
 {"sys:segdetach",  pp_sys_segdetach,  0, 0, { ___,___,___ } },
 {"sys:segflush",   pp_sys_segflush,   0, 0, { ___,___,___ } },
 {"sys:segfree",    pp_sys_segfree,    0, 0, { ___,___,___ } },
 {"sys:semacquire", pp_sys_semacquire, 0, 0, { ___,___,___ } },
 {"sys:semrelease", pp_sys_semrelease, 0, 0, { ___,___,___ } },
#endif
 {"sys:sleep",      pp_sys_sleep,      1, 1, { INT,___,___ } },
 {"sys:stat",       pp_sys_stat,       1, 1, { STR,___,___ } },
#ifdef FOO
 {"sys:sysr1",      pp_sys_sysr1,      0, 0, { ___,___,___ } },
#endif
 {"sys:unmount",    pp_sys_unmount,    2, 2, { STR,STR,___ } },
 {"sys:wait",       pp_sys_wait,       0, 0, { ___,___,___ } },
 {"sys:waitpid",    pp_sys_waitpid,    0, 0, { ___,___,___ } },
 {"sys:write",      pp_sys_write,      2, 2, { INT,STR,___ } },
 {"sys:wstat",      pp_sys_wstat,      2, 2, { STR,STR,___ } },

 /* Not syscalls but required... */
 {"sys:command-line",pp_sys_command_line,0,0,{ ___,___,___ } },

 {NULL}
};

#define xcat(a,b)	a##b
#define cat(a,b)	xcat(a,b)
#define mksym(x)	cat(x,_sym) = symbol_ref(#x)

void sys_init(void) {
	//signal(SIGPIPE, SIG_IGN); //XXX ignore notes?
	
	mksym(afid);
	mksym(aqid);
	mksym(aname);
	mksym(atime);
	/* mksym(count); */
	mksym(data);
	mksym(dev);
	mksym(ename);
	mksym(fid);
	mksym(gid);
	mksym(length);
	mksym(mode);
	mksym(msize);
	mksym(mtime);
	mksym(muid);
	mksym(name);
	mksym(newfid); 
	mksym(offset);
	mksym(oldtag);
	mksym(perm);
	mksym(qid);
	mksym(stat);
	mksym(type);
	mksym(uid);
	mksym(uname);
	mksym(wname);

	mksym(dir);
	mksym(Rattach);
	mksym(Rauth);
	mksym(Rclunk);
	mksym(Rcreate);
	mksym(Rerror);
	mksym(Rflush);
	mksym(Ropen);
	mksym(Rread);
	mksym(Rremove);
	mksym(Rstat);
	mksym(Rversion);
	mksym(Rwalk);
	mksym(Rwrite);
	mksym(Rwstat);
	mksym(Tattach);
	mksym(Tauth);
	mksym(Tclunk);
	mksym(Tcreate);
	mksym(Tflush);
	mksym(Topen);
	mksym(Tread);
	mksym(Tremove);
	mksym(Tstat);
	mksym(Tversion);
	mksym(Twalk);
	mksym(Twrite);
	mksym(Twstat);

	s9_add_image_vars(Plan9_image_vars);
	add_primitives("sys-plan9", Plan9_primitives);
}
